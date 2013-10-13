module HaskellGame.Camera where

import HaskellGame.Geometry

W = 640
H = 480

type Pos = (Int , Int)
type Width = Int
type Height = Int

data FixedCamera = {pos :: Pos, width :: Width , height :: Height}
data ChaseCamera = {pos :: Pos, width :: Width , height :: Height , fixCam :: FixedCamera}

gameToScreen :: FixedCamera -> Int -> Int -> (Int , Int) -> Maybe (Int , Int)
gameToScreen fc@((cx , cy) cw ch) sw sh (px , py) = case (x' , y') in
    x' <= sw && y' <= sh -> Just (x' , y')
    _                    -> Nothing
    where (x' , y') = (((px - (cx - div cw 2)) * (sw / cw) , (py - (cy - div ch 2)) * (sh / ch)))

screenToGame :: Fixed Camera -> Int -> Int -> (Int , Int) -> (Int , Int)
screenToGame fc@((cx, cy) cw ch) sw sh (gx , gy) = (x' , y')
  where (x' , y') = ((gx / (sw , cw) + (cx - div cw 2)) , (ch / sh) * (gy / (sh , ch) + (cy - div ch 2)))
