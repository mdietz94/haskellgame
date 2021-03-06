module Switch where

import Graphics.UI.SDL(Surface)
import Base.GraphicsManager(drawRect)
import Base.Geometry(Shape(Rectangle),collides)
import Base.Camera(FixedCamera,shapeToScreen)
import Player(Player, _bounding)


data Switch a = Tap { tapBounding :: Shape, tapOn :: (a -> a), tapOff :: (a -> a), tapHeld :: Bool }
	| Hold { holdBounding :: Shape, holdOn :: (a -> a), holdOff :: (a -> a) }

update :: a -> [Player] -> Switch a -> (Switch a,a)
update lD players t@(Tap r on off held) = if held == (not . null . filter (==True) . map ((collides r) . _bounding) $ players)
	then (t, (if held then on else off) $ lD)
	else (t { tapHeld=(not held) }, (if held then off else on) $ lD)
update lD players t@(Hold r on off) = if (null . filter (==True) . map ((collides r) . _bounding) $ players)
	then (t,on lD)
	else (t,off lD)


draw :: Surface -> FixedCamera -> Switch a -> IO ()
draw screen cam (Tap rect@(Rectangle x y w h) _ _ _) = do
    let rect' = shapeToScreen cam rect 640 480
    case rect' of
        Just (Rectangle x' y' w' h') -> drawRect screen (x' , y') w' h' (100,10,10)
        _ -> return ()
    return ()
draw screen cam (Hold rect@(Rectangle x y w h) _ _) = do
    let rect' = shapeToScreen cam rect 640 480
    case rect' of
        Just (Rectangle x' y' w' h') -> drawRect screen (x' , y') w' h' (100,10,10)
        _ -> return ()
    return ()
