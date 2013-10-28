{-# LANGUAGE TemplateHaskell #-}

module Platform where

import Base.GraphicsManager (drawRect)
import Base.Geometry (Shape(..),collides,x,y)
import Base.Camera
import Graphics.UI.SDL (Surface)
import Control.Lens

data Platform =
	Platform { _bounding :: Shape, color :: (Int,Int,Int) } |
	MoveablePlatform {
		start :: (Int,Int),
		end :: (Int,Int),
		_bounding :: Shape,
		_vel :: (Int,Int),
		mColor :: (Int,Int,Int),
		_forwards :: Bool
	} deriving Show

vel :: Lens' Platform (Int,Int)
vel = lens _vel (\shape v -> shape { _vel = v })

bounding :: Lens' Platform Shape
bounding = lens _bounding (\shape v -> shape { _bounding = v })

forwards :: Lens' Platform Bool
forwards = lens _forwards (\shape v -> shape { _forwards = v })

moveableInitialize :: (Int,Int) -> (Int,Int) -> Int -> Int -> Int -> (Int,Int,Int) -> Platform
moveableInitialize start@(x,y) end@(ex,ey) width height speed color = MoveablePlatform start end (Rectangle x y width height) (dx,dy) color True
    where
        (dx,dy) = (\(a,b) -> (ceiling . (* (fromIntegral speed)) $ a, (ceiling . (* (fromIntegral speed)) $ b))) . normalize $ (ex-x,ey-y)

{-

We need to make the turnaround much better, since it will move past it in this instance.
A check to see if the current direction makes us closer or further away is probably a much better
idea than making the platform hit the target exactly.  Or we could create a 10x10 box around
the target so that we would at least definitely hit it.  Both are probably equally slow/fast.

-}

update :: Platform -> Platform
update = move

draw :: Surface -> FixedCamera -> Platform -> IO ()
draw screen cam (Platform rect color) = do
    let rect' = shapeToScreen cam rect 640 480
    case rect' of
        Just (Rectangle x' y' w' h') -> drawRect screen (x' , y') w' h' color
        _ -> return ()
    return ()
draw screen cam (MoveablePlatform _ _ rect _ color _) = do
    let rect' = shapeToScreen cam rect 640 480
    case rect' of
        Just (Rectangle x' y' w' h') -> drawRect screen (x' , y') w' h' color
        _ -> return ()
    return ()

move :: Platform -> Platform
move mp@(MoveablePlatform (sx,sy) (ex,ey) _ (dx,dy) _ forward ) = (bounding .~ newRect) . (forwards .~ fwd) $ mp
    where
        newRect = ( x .~ (mp^.bounding.x + dx') ) . ( y .~ (mp^.bounding.y + dy') ) $ mp^.bounding
        (dx',dy') = both *~ (if fwd then 1 else (-1)) $ mp^.vel
        fwd = if mp^.forwards
            then (abs(mp^.bounding.x+dx-ex) + abs(mp^.bounding.y+dy-ey)) < (abs(mp^.bounding.x-ex) + abs(mp^.bounding.y-ey))
            else ((abs(mp^.bounding.x+dx-sx) + abs(mp^.bounding.y+dy-sy)) < abs(mp^.bounding.x-sx) + abs(mp^.bounding.y-sy))
move p@(Platform _ _) = p

normalize :: (Int,Int) -> (Float,Float)
normalize (x,y) = (dx,dy)
	where
		dy = (fromIntegral y) / mult
		dx = (fromIntegral x) / mult
		mult = sqrt . fromIntegral $ x*x + y*y
