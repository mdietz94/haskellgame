module Platform where

import Base.GraphicsManager (drawRect)
import Base.Geometry (Shape(..),collides)
import Base.Camera
import Graphics.UI.SDL (Surface)

data Platform =
	Platform { bounding :: Shape, color :: (Int,Int,Int) } |
	MoveablePlatform {
		start :: (Int,Int),
		end :: (Int,Int),
		mBounding :: Shape,
		vel :: (Int,Int),
		mColor :: (Int,Int,Int),
		forwards :: Bool
	} deriving Show

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
draw screen cam (Platform (Rectangle x y w h)  color) = do
    let pt = gameToScreen cam 640 480 (x,y)
    let wh = gameToScreen cam 640 480 (w,h)
    case (pt,wh) of
        (Just x', Just (w',h')) -> drawRect screen x' w' h' color
        _ -> return ()
    return ()
draw screen cam (MoveablePlatform _ _ (Rectangle x y w h) _ color _) = do
    let pt = gameToScreen cam 640 480 (x,y)
    let wh = gameToScreen cam 640 480 (w,h)
    case (pt,wh) of
        (Just x', Just (w',h')) -> drawRect screen x' w' h' color
        _ -> return ()
    return ()

move :: Platform -> Platform
move mp@(MoveablePlatform (sx,sy) (ex,ey) (Rectangle x y _ _) (dx,dy) _ forward ) = mp { mBounding=newRect, forwards=fwd }
    where
        newRect = (mBounding mp) { rectX=x+dx', rectY=y+dy' }
        (dx',dy') = (dx * (if fwd then 1 else (-1)), dy * (if fwd then 1 else (-1)))
        fwd = if forward then (abs(x+dx-ex) + abs(y+dy-ey)) < (abs(x-ex) + abs(y-ey)) else ((abs(x+dx-sx) + abs(y+dy-sy)) < abs(x-sx) + abs(y-sy))
move p@(Platform _ _) = p

normalize :: (Int,Int) -> (Float,Float)
normalize (x,y) = (dx,dy)
	where
		dy = (fromIntegral y) / mult
		dx = (fromIntegral x) / mult
		mult = sqrt . fromIntegral $ x*x + y*y