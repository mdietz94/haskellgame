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
  | rx' > sw || (rx' + rw') < 0 || ry' > sh || (ry' + rh') < 0 = Nothing
  | otherwise = Just $ Geo.Rectangle rx' ry' rw' rh'
  where
    (rx' , ry') = pointToScreen fc sw sh (rx , ry)  --find the location of the points  
    rw' = lengthToScreen fw fh sw sh rw                 --find length of height and width on screen
    rh' = lengthToScreen fw fh sw sh rh
shapeToScreen fc@(FixedCamera (fx , fy) fw fh) l@(Geo.Line (x0 , y0 ) (x1 , y1)) sw sh
  | (x0' >= 0 && x0' <= sw && y0' >= 0 && y0' <= sh)  || 
    (x1' >= 0 && x1' <= sw && y1' >= 0 && y1' <= sh) = Just $ Geo.Line (x0' , y0') (x1' , y1')
  | otherwise = Nothing
  where
    (x0' , y0') = pointToScreen fc sw sh (x0 , y0)
    (x1' , y1') = pointToScreen fc sw sh (x1 , y1)

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
  | fx' >= 0 && fx1' <= gw && fy' >= 0 && fy1' <= gh = FixedCamera (fx' , fy') fw fh  --stays
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
