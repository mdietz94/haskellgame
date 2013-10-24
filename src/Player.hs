module Player where

import Base.GraphicsManager
import Base.InputHandler
import Base.Geometry
import Base.Camera
import qualified Platform as P
import Graphics.UI.SDL (SDLKey(..), Surface)
import Control.Monad.State

data Player = Player { bounding :: Shape, velocity :: (Int,Int), alive :: Bool, image :: IO Surface }
playerHeight = 20
playerWidth = 20

initialize :: (Int,Int) -> Player
initialize (x,y) = Player (Rectangle x y playerWidth playerHeight) (0,2) True (loadImage "dot.bmp" Nothing)

update :: KeyboardState -> [Shape] -> [P.Platform] -> Player -> Player
update kS rects mp p = snd $ runState (updateS kS rects mp) p

updateS :: KeyboardState -> [Shape] -> [P.Platform] -> State Player ()
updateS kS rects mp = do
    player@(Player (Rectangle x y _ _) (dx,dy) _  _) <- get
    put player { velocity=(if strafe == 0 then ((if dx > 0 then floor else ceiling) $ ((fromIntegral dx)*0.9)) else (if dx*strafe > 0 then max (min (dx+strafe) 5) (-5) else strafe),dy) }
    modify moveV
    curP <- get
    let (nP,oG) = (moveObjs mp) . (checkCollisionV rects y) $ curP
    put $ (checkCollisionH rects x) . moveH $ nP
    if oG && (isPressed kS SDLK_UP) then modify (\p -> p { velocity=(fst . velocity $ p,(min 0 (snd . velocity $ p)) - 10 ) }) else return ()
    where
        strafe = (if isDown kS SDLK_LEFT then -1 else 0) + (if isDown kS SDLK_RIGHT then 1 else 0)

draw :: Surface -> FixedCamera -> Player -> IO ()
draw screen cam (Player (Rectangle x y _ _)  _ _ image) = do
    img <- image
    let pt = shapeToScreen cam (Point x y) 640 480
    case pt of
        Just (Point x' y') -> do
            drawImage screen img (x' , y')
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

checkCollisionH :: [Shape] -> Int -> Player -> Player
checkCollisionH [] _ p = p
checkCollisionH (r:rs) origX p@(Player pr@(Rectangle x y w h) (dx,dy) a i)
    | r `collides` (pr { rectX=origX }) = if (rectX r)-origX > 0
        then if (x+dx) < (rectX r)-w-1 then Player (pr { rectX=((rectX r)-w-1) }) (dx,dy) a i else p
        else if (x+dx) > (rectX r)-w-1 then Player (pr { rectX=((rectX r)+(rectW r)+1) }) (dx,dy) a i else p
    | dx == 0 = p
    | dx > 0 = if r `collides` (pr { rectX=origX, rectW=(x-origX+w) })
        then Player (pr { rectX=((rectX r)-w-1) }) (dx,dy) a i
        else checkCollisionH rs origX p
    | otherwise = if r `collides` (pr { rectW=(origX-x) })
        then Player (pr { rectX=((rectX r) + (rectW r) + 1)}) (dx,dy) a i
        else checkCollisionH rs origX p

checkCollisionV :: [Shape] -> Int -> Player -> (Player,Bool)
checkCollisionV [] _ p = (p,False)
checkCollisionV (r:rs) origY p@(Player pr@(Rectangle x y w h) (dx,dy) a i)
    | r `collides` (pr { rectY=origY }) = if (rectY r)-origY < 0
        then if (y+dy) < (rectY r)+(rectH r)+1 then (Player (pr { rectY=((rectY r) + (rectH r) + 1)}) (dx,2) a i, False) else (p,False)
        else if (y+dy) > (rectY r)-h-1 then (Player (pr { rectY=((rectY r) - h  - 1) }) (dx,2) a i, True) else (p,False)
    | dy == 0 = (p, False)
    | dy > 0 = if r `collides` (pr { rectY=origY, rectH=(y-origY+h) })
        then (Player (pr { rectY=((rectY r) - h  - 1) }) (dx,2) a i, True)
        else checkCollisionV rs origY p
    | otherwise = if r `collides` (pr { rectH=(origY-y) })
        then (Player (pr { rectY=((rectY r) + (rectH r) + 1)}) (dx,2) a i, False)
        else checkCollisionV rs origY p

moveObjs :: [P.Platform] -> (Player,Bool) -> (Player,Bool)
moveObjs ((P.MoveablePlatform _ _ r (dx',dy') _ fwd):rs) (p@(Player rect@(Rectangle x y _ _) (vx,vy) _ _ ),b)
    | r `collides` rect { rectY=y+3 } = (p { bounding=newRect, velocity=(vx,dy) },True)
    | otherwise = moveObjs rs (p,b)
        where
            dx = if fwd then dx' else dx'*(-1)
            dy = if fwd then dy' else dy'*(-1)
            newRect = rect { rectX=x+dx }
moveObjs _ p = p
