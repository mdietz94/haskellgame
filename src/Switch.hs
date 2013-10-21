module Switch where

import Graphics.UI.SDL(Surface)
import Base.GraphicsManager(drawRect)
import Base.Geometry(Shape(Rectangle),collides)
import Base.Camera(FixedCamera,gameToScreen)
import Level(LevelData(..))
import Player(Player,bounding)


data Switch = Tap { tapBounding :: Shape, tapOn :: (LevelData -> LevelData), tapOff :: (LevelData -> LevelData), tapHeld :: Bool }
	| Hold { holdBounding :: Shape, holdOn :: (LevelData -> LevelData), holdOff :: (LevelData -> LevelData) }

update :: LevelData -> Switch -> (Switch,LevelData)
update lD@(LevelData _ _ players _ _) t@(Tap r on off held) = if held == (not . null . filter (==True) . map ((collides r) . bounding) $ players)
	then (t,lD)
	else (t { tapHeld=(not held) }, (if held then off else on) $ lD)
update lD@(LevelData _ _ players _ _) t@(Hold r on off) = if (null . filter (==True) . map ((collides r) . bounding) $ players)
	then (t,on lD)
	else (t,off lD)


draw :: Surface -> FixedCamera -> Switch -> IO ()
draw screen cam (Tap (Rectangle x y w h) _ _ _) = do
    let pt = gameToScreen cam 640 480 (x,y)
    let wh = gameToScreen cam 640 480 (w,h)
    case (pt,wh) of
        (Just x', Just (w',h')) -> drawRect screen x' w' h' (10,10,10)
        _ -> return ()
    return ()
draw screen cam (Hold (Rectangle x y w h) _ _) = do
    let pt = gameToScreen cam 640 480 (x,y)
    let wh = gameToScreen cam 640 480 (w,h)
    case (pt,wh) of
        (Just x', Just (w',h')) -> drawRect screen x' w' h' (10,10,10)
        _ -> return ()
    return ()