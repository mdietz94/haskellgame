module Base.Camera where

--import HaskellGame.Geometry

type Point = (Int , Int)
type Width = Int
type Height = Int

--cameras a left-up corner and a height and width
data FixedCamera = FixedCamera { corner :: Point , width :: Width , height :: Height }


-- Translates a point from the board to the screen
gameToScreen :: FixedCamera -> Int -> Int -> (Int , Int) -> Maybe (Int , Int)
gameToScreen fc@(FixedCamera (fx , fy) fw fh) sw sh (px , py)
  | x' <= sw' && x' >= 0 && y' <= sh' && y' >= 0 = Just (ceiling x' , ceiling y')
  | otherwise = Nothing
  where 
    px' = fromIntegral px :: Float
    fx' = fromIntegral fx :: Float
    sw' = fromIntegral sw :: Float
    fw' = fromIntegral fw :: Float
    py' = fromIntegral py :: Float
    fy' = fromIntegral fy :: Float
    sh' = fromIntegral sh :: Float
    fh' = fromIntegral fh :: Float
    (x' , y') = ((px' - fx') * (fh' / fw' * sh' / sw') , (py' - fy') * (fh' / fw' * sh' / sw'))

--Translates a point from the screen to the board
screenToGame :: FixedCamera -> Int -> Int -> (Int , Int) -> (Int , Int)
screenToGame fc@(FixedCamera (fx , fy) fw fh) sw sh (gx , gy) = (ceiling x' , ceiling y')
  where
    gx' = fromIntegral gx :: Float
    fx' = fromIntegral fx :: Float
    sw' = fromIntegral sw :: Float
    fw' = fromIntegral fw :: Float
    gy' = fromIntegral gy :: Float
    fy' = fromIntegral fy :: Float
    sh' = fromIntegral sh :: Float
    fh' = fromIntegral fh :: Float
    (x' , y') = ((gx' / (sw' / fw') + fx') , (gy' / (sh' / fh') + fy'))

fixedCamDoesCollideX :: FixedCamera -> Int -> Int -> Bool
fixedCamDoesCollideX fc@(FixedCamera (fx , fy) fw fh) x sw = (fx + x <= 0) || (fx + fw + x >= sw)

fixedCamDoesCollideY :: FixedCamera -> Int -> Int -> Bool
fixedCamDoesCollideY fc@(FixedCamera (fx , fy) fw fh) y sh = (fy + y <= 0) || (fy + fh + y >= sh)

moveFixedCamX :: FixedCamera -> Int -> Int -> FixedCamera
moveFixedCamX fc@(FixedCamera (fx , fy) fw fh) x sw
  | fixedCamDoesCollideX fc x sw = fc
  | otherwise = FixedCamera (fx+x,fy) fw fh

moveFixedCamY :: FixedCamera -> Int -> Int -> FixedCamera
moveFixedCamY fc@(FixedCamera (fx , fy) fw fh) y sh 
  | fixedCamDoesCollideY fc y sh = fc
  | otherwise = FixedCamera (fx , fy + y) fw fh


data ChaseCamera = ChaseCamera {chaseCamCorner :: Point, chaseCamWidth :: Width , chaseCamHeight :: Height , fixCam :: FixedCamera}



{-
data Camera = FixedCamera | ChaseCamera

-- Translates a point from the board to the screen
gameToScreen :: ChaseCamera -> Int -> Int -> (Int , Int) -> Maybe (Int , Int)
gameToScreen cc@((cx , cy) cw ch fcam) sw sh (px , py) = case (x' , y') in
    x' <= sw && y' <= sh -> Just (x' , y')
    _                    -> Nothing
    where (x' , y') = (((px - cx) * (sw / cw) , (py - cy) * (sh / ch)))

--Translates a point from the screen to the board
screenToGame :: ChaseCamera -> Int -> Int -> (Int , Int) -> (Int , Int)
screenToGame cc@((cx , cy) cw ch fcam) sw sh (gx , gy) = (x' , y')
  where (x' , y') = ((gx / (sw , cw) + cx) , (ch / sh) * (gy / (sh , ch) + cy))

--Moves chase camera in x-direction and y-direction
moveChaseCamera :: ChaseCamera -> Int -> Int -> ChaseCamera
moveChaseCamera cc@((ccx , ccy) ccw cch fc@((fcx , fcy) fcw fch)) dx dy = case (fcx' , fcy') of
  fcx'+ fcw <= ccx + ccw

--Moves fixed camera in x-direction and y-direction
moveFixedCamera :: FixedCamera -> Int -> Int -> FixedCamera
moveFixedCamera fc@((cx , cy) cw ch) x y = ChaseCamera ((cx + x , cy + y) cw ch)
-}


