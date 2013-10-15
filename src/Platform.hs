module Platform where

import Base.GraphicsManager (drawRect)
import Base.Geometry (Shape(..))
import Base.Camera
import Graphics.UI.SDL (Surface)

data Platform = Platform { bounding :: Shape, color :: (Int,Int,Int) }

draw :: Surface -> FixedCamera -> Platform -> IO ()
draw screen cam (Platform (Rectangle x y w h)  color) = do
    let pt = gameToScreen cam 640 480 (x,y)
    let wh = gameToScreen cam 640 480 (w,h)
    case (pt,wh) of
        (Just x', Just (w',h')) -> drawRect screen x' w' h' color
        _ -> return ()
    return ()
