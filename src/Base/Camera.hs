module Base.Camera where

import qualified Base.Geometry as Geo

type Point = (Int , Int)
type Width = Int
type Height = Int

--cameras a left-up corner and a height and width
data FixedCamera = FixedCamera { corner :: Point , width :: Width , height :: Height }

-- Translates a point from the board to the screen
pointToScreen :: FixedCamera -> Width -> Height -> Point -> Point
pointToScreen fc@(FixedCamera (fx , fy) fw fh) sw sh (px , py) = (ceiling x' , ceiling y')
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

--Translates any shape from Base.Geometry to screen
shapeToScreen :: FixedCamera -> Geo.Shape -> Int -> Int -> Maybe Geo.Shape
shapeToScreen fc@(FixedCamera (fx , fy) fw fh) p@(Geo.Point px py) sw sh 
  | px' < 0 || px' > sw || py' < 0 || py' > sh = Nothing 
  | otherwise = Just $ Geo.Point px' py'
  where
    (px' , py') = pointToScreen fc sw sh (px , py)
shapeToScreen fc@(FixedCamera (fx , fy) fw fh) r@(Geo.Rectangle rx ry rw rh) sw sh
  | not (((rx' >= 0 && rx' <= sw) || ((rx' + rw' >= 0) && (rx' + rw' <= sw))) &&
         ((ry' >= 0 && ry' <= sh) || ((ry' + rh' >= 0) && (ry' + rh' <= sh)))) = Nothing --none on screen
  | rx' >= 0 && (rx' + rw' <= sw) && ry' >= 0 && (ry' + rh' <= sh) = Just $ Geo.Rectangle rx' ry' rw' rh' --all on screen
  | otherwise = Just $ Geo.Rectangle rx'' ry'' rw'' rh'' --some on screen
  where
    (rx' , ry') = pointToScreen fc sw sh (rx , ry)  --find the location of the points  
    rw' = lengthToScreen fw fh sw sh rw                 --find length of height and width on screen
    rh' = lengthToScreen fw fh sw sh rh
    rx'' = fitXtoScreen rx' sw          
    ry'' = fitYtoScreen ry' sh
    rw'' = fitLengthToScreen rx' rw' fw
    rh'' = fitLengthToScreen ry' rh' fh
shapeToScreen fc@(FixedCamera (fx , fy) fw fh) l@(Geo.Line (x0 , y0 ) (x1 , y1)) sw sh
  | (lineIntersects l' st) || (lineIntersects l' sr) || 
    (lineIntersects l' sb) || (lineIntersects l' sl) || (x0' >= 0 && x0' <= sw) = Just l'
  | otherwise = Nothing
  where
    l' = Geo.Line (x0' , y0') (x1' , y1')
    st = Geo.Line (0 , 0) (sw , 0)
    sr = Geo.Line (sw , 0) (sw , sh)
    sb = Geo.Line (0 , sh) (sw , sh)
    sl = Geo.Line (0 , 0) (0 , sh)
    (x0' , y0') = pointToScreen fc sw sh (x0 , y0)
    (x1' , y1') = pointToScreen fc sw sh (x1 , y1)

lineIntersects :: Geo.Shape -> Geo.Shape -> Bool
lineIntersects l0@(Geo.Line (x0 , y0) (x0' , y0')) l1@(Geo.Line (x1 , y1) (x1' , y1')) = (dx0 * dy1 - dx1 * dy0) /= 0
  where
    dx0 = fromIntegral $ x0' - x0
    dy0 = fromIntegral $ y0' - y0
    dx1 = fromIntegral $ x1' - x1
    dy1 = fromIntegral $ y1' - y1

--Fits a rectangle to the screen. Given rect must be already adjusted to screen.
fitShapeToScreen :: Geo.Shape -> Width -> Height -> Geo.Shape
fitShapeToScreen r@(Geo.Rectangle rx ry rw rh) sw sh = Geo.Rectangle rx' ry' rw' rh'
  where
    rx' = fitXtoScreen rx sw
    ry' = fitYtoScreen ry sh
    rw' = fitLengthToScreen rx rw sw
    rh' = fitLengthToScreen ry rh sh

--Fits a length to the screen.
fitLengthToScreen :: Int -> Int -> Int -> Int
fitLengthToScreen rp rd sd | rp < 0 = rd + rp | rp + rd > sd = sd - rp | otherwise = rd

--Fits a x value to the screen. x must be already adjusted to screen
fitXtoScreen :: Int -> Width -> Int
fitXtoScreen x sh | x < 0 = 0 | otherwise = x

--fits a y value to screen. y must already be adjusted to screen
fitYtoScreen :: Int -> Height -> Int
fitYtoScreen y sh | y < 0 = 0 | otherwise = y

--Translates a length to screen. 
lengthToScreen :: Width -> Height -> Width -> Height -> Int -> Int
lengthToScreen fw fh sw sh l = ceiling (l' * (fh' / sh' * fw' / sw'))
  where
    l' = fromIntegral l :: Float
    fw' = fromIntegral fw :: Float
    sw' = fromIntegral sw :: Float
    fh' = fromIntegral fh :: Float
    sh' = fromIntegral sh :: Float

--Move fixed camera
fixedCamMoveTo :: FixedCamera -> Point -> Width -> Height -> FixedCamera
fixedCamMoveTo (FixedCamera (fx , fy) fw fh) (x , y) gw gh
  | fx' >= 0 && fx1' <= gw && fy' >= 0 && fy1' <= sh = FixedCamera (fx' , fy') fw fh  --stays
  | fx' < 0 && fy' < 0 = FixedCamera (0 , 0) fw fh                            --top left
  | fx' < 0 && fy1' <= gh = FixedCamera (0 , fy') fw fh                       --left
  | fx' < 0 && fy1' > gh = FixedCamera (0 , gh - fh) fw fh                    --bottom left
  | fx1' <= gw && fy' < 0 = FixedCamera (fx' , 0) fw fh                       --top
  | fx1' <= gw && fy1' > gh = FixedCamera (fx' , gh - fh) fw fh               --bottom
  | fx1' > gw && fy1'> gh = FixedCamera (gw - fw , gh - fh) fw fh             --bottom right
  where
    fx' = fx + x
    fx1' = fx + fw + x
    fy' = fy + y
    fy1' = fy + fh + y


{-
data ChaseCamera = ChaseCamera {chaseCamCorner :: Point, chaseCamWidth :: Width , chaseCamHeight :: Height , fixCam :: FixedCamera}





--Finds the length a line must be so it does not go off edge of screen.
--Takes a x|y coordinate, a length, and a camera dimension
fitLineToScreen :: Int -> Int -> Int -> Int
fitLineToScreen xy l fd 
  | xy + l < 0  = xy      --If line extends past top or left edge
  | xy + l > fd = fd - xy --If line extends past right or down edge
  | otherwise   = l       --Line is in screen

--Finds location of a point on the screen
--takes camera, screen dimensions, x and y of point
pointToScreen :: FixedCamera -> Int -> Int -> (Int , Int) -> (Int , Int)
pointToScreen fc@(FixedCamera (fx , fy) fw fh) sw sh (x , y) = (ceiling x'' , ceiling y'')
  where
    x'  = fromIntegral x :: Float 
    y'  = fromIntegral y :: Float
    fx' = fromIntegral fx :: Float
    fy' = fromIntegral fy :: Float
    fw' = fromIntegral fw :: Float
    fh' = fromIntegral fh :: Float
    sw' = fromIntegral sw :: Float
    sh' = fromIntegral sh :: Float
    (x'' , y'') = ((x' - fx') * (fh' / fw' * sh' / sw') , (y' - fy') * (fh' / fw' * sh' / sw'))

--Finds the intersection of two lines
intersection :: Geo.Line -> Geo.Line -> Maybe (Int , Int)
intersection m@(Geo.Line (mx0 , my0) (my1 , my1)) n@(Geo.Line (nx0 ,ny0) (nx1 , ny1))
  | delta == 0 = Nothing                -- lines are parrallel or same line, in both cases nothing
  | otherwise  = Just (x , y)
  where
    mslope = (my1 - my0) / (mx1 - mx0)
    nslope = (ny1 - ny0) / (nx1 - nx0)
    mA     = my1 - my0
    mB     = mx0 - mx1
    mC     = mA * mx1 + mB * my1
    nA     = ny1 - ny0
    nB     = nx0 - nx1
    nC     = nA * nx1 + nB * ny1
    delta  = mA * nB - mB * nA
    x      = (nB * mC - mB * nC) / delta  
    y      = (mA * nC - nA * mC) / delta
    
    
--Checks if a point is on the screen. Takes screen coords of pt and screen dimensions.
isPointOnScreen :: (Int , Int) -> Int -> Int -> Bool
isPointOnScreen x y sw sh = x >= 0 && x <= sw && y >= 0 && y <= sh

--Fits a line to the screen.
--takes the coordinates of the line and returns coordinates of a line that does not run off screen
fitLineOnScreen :: Geo.Line -> Int -> Int -> Maybe Geo.Line (Int , Int) (Int , Int)
fitLineOnScreen l@(Geo.Line (x0 , y0) (x1 , y1)) sw sh 
  | isPointOnScreen x0 && isPointOnScreen x1 && isPointOnScreen y0 && isPointOnScreen y1 = Just l      --line is on screen
  | intersectL == Nothing && intersectT == Nothing && intersectR == Nothing && intersectB == Nothing = Nothing
  | intersectT != Nothing && intersectB != Nothing = 
  where
    intersectL = intersection (Geo.Line (0 , 0) (0 , sh)) l
    intersectT = intersection (Geo.Line (0 , 0) (sw , 0)) l
    intersectR = intersection (Geo.Line (sw , 0) (sw , sh)) l
    intersectB = intersection (Geo.Line (sw , sh) (0 , sh)) l
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

--tests if the fixed camera collides with the board in the x-direction by testing against board
fixedCamDoesCollideX :: FixedCamera -> Int -> Int -> Bool
fixedCamDoesCollideX fc@(FixedCamera (fx , fy) fw fh) x gw = (fx + x <= 0) || (fx + fw + x >= gw)

--tests if the fixed camera collides with the board in the y-direction
fixedCamDoesCollideY :: FixedCamera -> Int -> Int -> Bool
fixedCamDoesCollideY fc@(FixedCamera (fx , fy) fw fh) y gh = (fy + y <= 0) || (fy + fh + y >= gh)

--moves the fixed camera x game units in the x-direction. Also requires game width
moveFixedCamX :: FixedCamera -> Int -> Int -> FixedCamera
moveFixedCamX fc@(FixedCamera (fx , fy) fw fh) x gw
  | fixedCamDoesCollideX fc x gw = fc
  | otherwise = FixedCamera (fx + x , fy) fw fh

--moves the fixed camera y game units in the y-direction. Requires game height
moveFixedCamY :: FixedCamera -> Int -> Int -> FixedCamera
moveFixedCamY fc@(FixedCamera (fx , fy) fw fh) y gh 
  | fixedCamDoesCollideY fc y gh = fc
  | otherwise = FixedCamera (fx , fy + y) fw fh





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


