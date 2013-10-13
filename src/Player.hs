module Player where

import Base.GraphicsManager
import Base.InputHandler
import Base.Geometry
import Base.Camera
import Graphics.UI.SDL (SDLKey(..), Surface)


data Player = Player { bounding :: Shape, velocity :: (Int,Int), alive :: Bool, image :: IO Surface }
playerHeight = 10
playerWidth = 5

initialize :: (Int,Int) -> Player
initialize (x,y) = Player (Rectangle x y playerWidth playerHeight) (0,2) True (loadImage "dot.bmp" Nothing)

update :: KeyboardState -> [Shape] -> Player -> Player
update kS rects player@(Player _ (dx,dy) _  _) = if isOnGround && (isPressed kS SDLK_UP) then player5 { velocity=(dx',-5) } else player5
    where
        (player5@(Player _ (dx',dy') _ _),isOnGround) = checkCollisionV rects player4
        player4 = moveV player3
        player3 = checkCollisionH rects player2
        player2 = moveH player1
        player1 = player { velocity=(strafe,dy) }
        strafe = (if isDown kS SDLK_LEFT then -5 else 0) + (if isDown kS SDLK_RIGHT then 5 else 0)

draw :: Surface -> Player -> FixedCamera -> IO ()
draw screen (Player (Rectangle x y _ _)  _ _ image) cam = do
    img <- image
    let pt = gameToScreen cam 640 480 (x,y)
    case pt of
        Just x' -> do
            putStrLn $ "(" ++ (show x) ++ "," ++ (show y) ++ "): " ++ (show x')
            drawImage screen img x'
        Nothing -> return True
    return ()

moveH :: Player -> Player
moveH p@(Player (Rectangle x _ _ _) (dx,_) _ _) = p { bounding=newRect }
    where
        newRect = (bounding p) { rectX=x+dx }

moveV :: Player -> Player
moveV p@(Player (Rectangle _ y _ _) (dx,dy) _ _) = p { bounding=newRect,velocity=(dx,dy+2) } 
    where
        newRect = (bounding p) { rectY=y+dy }

checkCollisionH :: [Shape] -> Player -> Player
checkCollisionH [] p = p
checkCollisionH (r:rs) p@(Player pr@(Rectangle x y w h) (dx,dy) a i)
    | dx == 0 = p
    | dx > 0 = if r `isRight` pr
        then Player (pr { rectX=((rectX r)-1) }) (0,dy) a i
        else checkCollisionH rs p
    | otherwise = if r `isLeft` pr
        then Player (pr { rectX=((rectX r) + (rectW r) + 1)}) (0,dy) a i
        else checkCollisionH rs p
checkCollisionV :: [Shape] -> Player -> (Player,Bool)
checkCollisionV [] p = (p,False)
checkCollisionV (r:rs) p@(Player pr@(Rectangle x y w h) (dx,dy) a i)
    | dy == 0 = (p, False)
    | dy > 0 = if r `isBelow` pr
        then (Player (pr { rectY=((rectY r) - h  - 1) }) (dx,2) a i, True)
        else checkCollisionV rs p
    | otherwise = if r `isOnTop` pr
        then (Player (pr { rectY=((rectY r) + (rectH r) + 1)}) (dx,2) a i, False)
        else checkCollisionV rs p
