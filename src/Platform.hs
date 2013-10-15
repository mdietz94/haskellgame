module Platform where

import Base.GraphicsManager (drawRect)
import Base.Geometry (Shape(..))
import Base.Camera
import Graphics.UI.SDL (Surface)

data Platform = Platform { bounding :: Shape, color :: (Int,Int,Int) }

draw :: Surface -> Platform -> FixedCamera -> IO ()
draw screen (Platform (Rectangle x y w h)  color) cam = do
    let pt = gameToScreen cam 640 480 (x,y)
    case pt of
        Just x' -> drawRect screen x' w h color
        Nothing -> return ()
    return ()
