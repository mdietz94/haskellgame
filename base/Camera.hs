module HaskellGame.Camera where

import HaskellGame.Geometry

W = 640
H = 480

type Pos = (Int, Int)
type Width = Int
type Height = Int

data FixedCamera = {pos :: Pos, width :: Width, height :: Height}
data ChaseCamera = {pos :: Pos, width :: Width, 
                    height :: Height, fixCam :: FixedCamera}

gameToScreen :: FixedCamera -> Int -> Int -> (Int, Int) -> Maybe (Int, Int)
gameToScreen ((x,y), wd, ht) w h (a, b) = case (c , d) in
    c <= w && b <= h -> Just c b
    _                -> Nothing
    where (c , d) = (w / wd * a ,  h / ht * b)
