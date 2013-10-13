module HaskellGame.Player where

import HaskellGame.GraphicsManager
import HaskellGame.InputHandler
import HaskellGame.Geometry
import Graphics.UI.SDL (SDLKey(..))

data Player = Player { bounding :: Shape, velocity :: (Int,Int), alive :: Bool } deriving Show
playerHeight = 10
playerWidth = 5

initialize :: (Int,Int) -> Player
initialize (x,y) = Player (Rectangle x y playerWidth playerHeight) (0,0) True

update :: KeyboardState -> [Shape] -> Player -> Player
update kS rects player@(Player _ (dx,dy) _ ) = if isOnGround && (isPressed kS SDLK_UP) then player5 { velocity=(dx',-10) } else player5
    where
        (player5@(Player _ (dx',dy') _),isOnGround) = checkCollisionV rects player4
        player4 = moveV player3
        player3 = checkCollisionH rects player2
        player2 = moveH player1
        player1 = player { velocity=(dx+strafe,dy+2) }
        strafe = (if isDown kS SDLK_LEFT then -5 else 0) + (if isDown kS SDLK_RIGHT then 5 else 0)

moveH :: Player -> Player
moveH (Player (Rectangle x y w h) (dx,dy) a) = (Player (Rectangle (x+dx) (y) w h) (dx,dy) a)

moveV :: Player -> Player
moveV (Player (Rectangle x y w h) (dx,dy) a) = (Player (Rectangle (x) (y+dy) w h) (dx,dy) a)

checkCollisionH :: [Shape] -> Player -> Player
checkCollisionH [] p = p
checkCollisionH (r:rs) p@(Player pr@(Rectangle x y w h) (dx,dy) a)
    | dx == 0 = p
    | dx > 0 = if r `isRight` pr
        then Player (pr { rectX=((rectX r)-1) }) (0,dy) a
        else checkCollisionH rs p
    | otherwise = if r `isLeft` pr
        then Player (pr { rectX=((rectX r) + (rectW r) + 1)}) (0,dy) a
        else checkCollisionH rs p
checkCollisionV :: [Shape] -> Player -> (Player,Bool)
checkCollisionV [] p = (p,False)
checkCollisionV (r:rs) p@(Player pr@(Rectangle x y w h) (dx,dy) a)
    | dy == 0 = (p, False)
    | dy > 0 = if r `isBelow` pr
        then (Player (pr { rectY=((rectY r)-1) }) (dx,2) a, True)
        else checkCollisionV rs p
    | otherwise = if r `isOnTop` pr
        then (Player (pr { rectY=((rectY r) + (rectH r) + 1)}) (dx,2) a, False)
        else checkCollisionV rs p