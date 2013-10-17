module Platform where

import Base.GraphicsManager (drawRect)
import Base.Geometry (Shape(..),collides)
import Base.Camera
import Graphics.UI.SDL (Surface)

data Platform = Platform { bounding :: Shape, color :: (Int,Int,Int) }

-- For now it is important that
-- (end-start)*speed will move from start to end evenly, because
-- otherwise we will jump over the end and never turn around
data MoveablePlatform = MoveablePlatform {
	start :: (Int,Int),
	end :: (Int,Int),
	mBounding :: Shape,
	speed :: Int,
	mColor :: (Int,Int,Int),
	forwards :: Bool
} deriving Show

{-

Should probably put MoveablePlatform and Platform together?  We can still separate
them in LevelData/LevelConfig but it will probably simplify things.

-}

initialize :: (Int,Int) -> (Int,Int) -> Int -> Int -> Int -> (Int,Int,Int) -> MoveablePlatform
initialize start@(x,y) end width height speed color = MoveablePlatform start end (Rectangle x y width height) speed color True

{-

We need to make the turnaround much better, since it will move past it in this instance.
A check to see if the current direction makes us closer or further away is probably a much better
idea than making the platform hit the target exactly.  Or we could create a 10x10 box around
the target so that we would at least definitely hit it.  Both are probably equally slow/fast.

-}

update :: [Shape] -> MoveablePlatform -> MoveablePlatform
update objs mp@(MoveablePlatform _ _ (Rectangle x y _ _) _ _ _ ) = mp2 { mBounding=mpr1 }
	where
		mpr1 = checkCollisionV objs y dy (mBounding mp2)
		(dy,mp2) = moveV $ mp1 { mBounding=mpr }
		mpr = checkCollisionH objs x dx (mBounding mp1)
		(dx,mp1) = moveH mp

draw :: Surface -> FixedCamera -> Platform -> IO ()
draw screen cam (Platform (Rectangle x y w h)  color) = do
    let pt = gameToScreen cam 640 480 (x,y)
    let wh = gameToScreen cam 640 480 (w,h)
    case (pt,wh) of
        (Just x', Just (w',h')) -> drawRect screen x' w' h' color
        _ -> return ()
    return ()

moveableDraw :: Surface -> FixedCamera -> MoveablePlatform -> IO ()
moveableDraw screen cam (MoveablePlatform _ _ (Rectangle x y w h) _ color _) = do
    let pt = gameToScreen cam 640 480 (x,y)
    let wh = gameToScreen cam 640 480 (w,h)
    case (pt,wh) of
        (Just x', Just (w',h')) -> drawRect screen x' w' h' color
        _ -> return ()
    return ()

{-

These functions should probably be generalized somewhere.  They do differ
slightly from the users' versions, but they are similar enough that a more
general version should be able to be created.

-}

moveH :: MoveablePlatform -> (Int,MoveablePlatform)
moveH mp@(MoveablePlatform (sx,sy) (ex,ey) (Rectangle x y _ _) speed _ forward ) = (dx,mp { mBounding=newRect, forwards=fwd })
    where
        newRect = (mBounding mp) { rectX=x+dx }
        dx = ceiling . (* (fromIntegral speed)) . fst . normalize $ (if fwd then (ex-sx,ey-sy) else (sx-ex,sy-ey))
        fwd = if forward then (x,y) /= (ex,ey) else (x,y) == (sx,sy)

normalize :: (Int,Int) -> (Float,Float)
normalize (x,y) = (dx,dy)
	where
		dy = (fromIntegral y) / mult
		dx = (fromIntegral x) / mult
		mult = sqrt . fromIntegral $ x*x + y*y

moveV :: MoveablePlatform -> (Int,MoveablePlatform)
moveV mp@(MoveablePlatform (sx,sy) (ex,ey) (Rectangle x y _ _) speed _ forward ) = (dy,mp { mBounding=newRect, forwards=fwd })
    where
        newRect = (mBounding mp) { rectY=y+dy }
        dy = ceiling . (* (fromIntegral speed)) . snd . normalize $ (if fwd then (ex-sx,ey-sy) else (sx-ex,sy-ey))
        fwd = if forward then (x,y) /= (ex,ey) else (x,y) == (sx,sy)

checkCollisionH :: [Shape] -> Int -> Int -> Shape -> Shape
checkCollisionH [] _ _ mp = mp
checkCollisionH (r:rs) origX dx pr@(Rectangle x y w h)
    | dx == 0 = pr
    | dx > 0 = if r `collides` (pr { rectX=origX, rectW=(x-origX+w) })
        then pr { rectX=((rectX r)-w-1) }
        else checkCollisionH rs origX dx pr
    | otherwise = if r `collides` (pr { rectW=(origX-x) })
        then pr { rectX=((rectX r) + (rectW r) + 1) }
        else checkCollisionH rs origX dx pr

checkCollisionV :: [Shape] -> Int -> Int -> Shape -> Shape
checkCollisionV [] _ _ pr = pr
checkCollisionV (r:rs) origY dy pr@(Rectangle x y w h)
    | dy == 0 = pr
    | dy > 0 = if r `collides` (pr { rectY=origY, rectH=(y-origY+h) })
        then pr { rectY=((rectY r) - h  - 1) }
        else checkCollisionV rs origY dy pr
    | otherwise = if r `collides` (pr { rectH=(origY-y) })
        then pr { rectY=((rectY r) + (rectH r) + 1) }
        else checkCollisionV rs origY dy pr
