module HaskellGame.Camera where

--import HaskellGame.Geometry

type Point = (Int , Int)
type Width = Int
type Height = Int

--cameras a left-up corner and a height and width
data FixedCamera = FixedCamera { corner :: Point , width :: Width , height :: Height }


-- Translates a point from the board to the screen
gameToScreen :: FixedCamera -> Int -> Int -> (Int , Int) -> Maybe (Int , Int)
gameToScreen fc@(FixedCamera (fx , fy) fw fh) sw sh (px , py)
  | x' <= sw && x' >= 0 && y' <= sh && y' >= 0 = Just (x' , y')
  | otherwise = Nothing
  where (x' , y') = (((px - fx) * (sw `div` fw) , (py - fy) * (sh `div` fh)))

--Translates a point from the screen to the board
screenToGame :: FixedCamera -> Int -> Int -> (Int , Int) -> (Int , Int)
screenToGame fc@(FixedCamera (fx , fy) fw fh) sw sh (gx , gy) = (x' , y')
  where (x' , y') = ((gx `div` (sw `div` fw) + fx) , (gy `div` (sh `div` fh) + fy))

{-
data ChaseCamera = {corner :: Point, width :: Width , height :: Height , fixCam :: FixedCamera}

data Camera = FixedCamera | ChaseCamera

-- Translates a point from the board to the screen
gameToScreen :: ChaseCamera -> Int -> Int -> (Int , Int) -> Maybe (Int , Int)
gameToScreen cc@((cx , cy) cw ch fcam) sw sh (px , py) = case (x' , y') in
    x' <= sw && y' <= sh -> Just (x' , y')
    _                    -> Nothing
    where (x' , y') = (((px - cx) * (sw `div` cw) , (py - cy) * (sh `div` ch)))

--Translates a point from the screen to the board
screenToGame :: ChaseCamera -> Int -> Int -> (Int , Int) -> (Int , Int)
screenToGame cc@((cx , cy) cw ch fcam) sw sh (gx , gy) = (x' , y')
  where (x' , y') = ((gx `div` (sw , cw) + cx) , (ch `div` sh) * (gy `div` (sh , ch) + cy))

--Moves chase camera in x-direction and y-direction
moveChaseCamera :: ChaseCamera -> Int -> Int -> ChaseCamera
moveChaseCamera cc@((ccx , ccy) ccw cch fc@((fcx , fcy) fcw fch)) dx dy = case (fcx' , fcy') of
  fcx'+ fcw <= ccx + ccw

--Moves fixed camera in x-direction and y-direction
moveFixedCamera :: FixedCamera -> Int -> Int -> FixedCamera
moveFixedCamera fc@((cx , cy) cw ch) x y = ChaseCamera ((cx + x , cy + y) cw ch)
-}


