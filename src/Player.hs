{-# LANGUAGE TemplateHaskell #-}

module Player where

import Base.GraphicsManager
import Base.InputHandler
import Base.Geometry
import Base.Camera
import qualified Platform as P
import Graphics.UI.SDL (SDLKey(..), Surface)
import Control.Monad.State
import Control.Lens

data Player = Player { _bounding :: Shape, _velocity :: (Int,Int), _alive :: Bool, _image :: IO Surface }
$( makeLenses ''Player )

playerHeight = 20
playerWidth = 20

initialize :: (Int,Int) -> Player
initialize (x,y) = Player (Rectangle x y playerWidth playerHeight) (0,2) True (loadImage "dot.bmp" Nothing)

update :: KeyboardState -> [Shape] -> [P.Platform] -> Player -> Player
update kS rects mp p = snd $ runState (updateS kS rects mp) p

updateS :: KeyboardState -> [Shape] -> [P.Platform] -> State Player ()
updateS kS rects mp = do
    x <- use $ bounding.rectX
    y <- use $ bounding.rectY
    velocity._1 %= (\dx -> if strafe == 0 then ((if dx > 0 then floor else ceiling) $ ((fromIntegral dx)*0.9)) else (if dx*strafe > 0 then max (min (dx+strafe) 5) (-5) else strafe))
    modify moveV
    curP <- get
    let (nP,oG) = (moveObjs mp) . (checkCollisionV rects y) $ curP
    put $ (checkCollisionH rects x) . moveH $ nP
    if oG && (isPressed kS SDLK_UP) then velocity._2 %= (\dy -> (min 0 dy) - 10 ) else return ()
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
moveH p = bounding.rectX +~ p^.velocity._1 $ p

moveV :: Player -> Player
moveV p = (bounding.rectY +~ p^.velocity._2) . (velocity._2 +~ 2) $ p

checkCollisionH :: [Shape] -> Int -> Player -> Player
checkCollisionH [] _ p = p
checkCollisionH (r:rs) origX p@(Player pr@(Rectangle x y w h) (dx,dy) a i)
    | r `collides` (rectX .~ origX $ p^.bounding) = if r^.rectX - origX > 0
        then if (p^.bounding.rectX + p^.velocity._1) < r^.rectX - p^.bounding.rectW -1 then ((bounding.rectX .~ (r^.rectX - p^.bounding.rectW - 1)) $ p) else p
        else if (x+dx) > (r^.rectX - p^.bounding.rectW - 1) then ((bounding.rectX .~ (r^.rectX + r^.rectW + 1)) $ p) else p
    | p^.velocity._1 == 0 = p
    | p^.velocity._1 > 0 = if r `collides` ((rectX .~ origX) . (rectW .~ (p^.bounding.rectX - origX + p^.bounding.rectW)) $ p^.bounding)
        then bounding.rectX .~ (r^.rectX - p^.bounding.rectW - 1) $ p
        else checkCollisionH rs origX p
    | otherwise = if r `collides` (rectW .~ (origX - p^.bounding.rectX) $ p^.bounding)
        then (bounding.rectX .~ r^.rectX + r^.rectW + 1) $ p
        else checkCollisionH rs origX p

checkCollisionV :: [Shape] -> Int -> Player -> (Player,Bool)
checkCollisionV [] _ p = (p,False)
checkCollisionV (r:rs) origY p@(Player pr@(Rectangle x y w h) (dx,dy) a i)
    | r `collides` (rectY .~ origY $ p^.bounding) = if (p^.bounding.rectY)-origY < 0
        then if (p^.bounding.rectY+p^.velocity._2) < (r^.rectY)+(r^.rectH)+1 then ((bounding.rectY .~ (r^.rectY + r^.rectH + 1)) . (velocity._2 .~ 2) $ p, False) else (p,False)
        else if (p^.bounding.rectY+p^.velocity._2) > (r^.rectY)-(r^.rectH)-1 then ((bounding.rectY .~ (r^.rectY - p^.bounding.rectH - 1)) . (velocity._2 .~ 2) $ p, True) else (p,False)
    | p^.velocity._2 == 0 = (p, False)
    | p^.velocity._2 > 0 = if r `collides` ((rectY .~ origY) . (rectH .~ ((p^.bounding.rectY) - origY + (p^.bounding.rectH))) $ p^.bounding)
        then ((bounding.rectY .~ r^.rectY - p^.bounding.rectH - 1) . (velocity._2 .~ 2) $ p, True)
        else checkCollisionV rs origY p
    | otherwise = if r `collides` (rectH .~ (origY - (p^.bounding.rectY)) $ p^.bounding)
        then ((bounding.rectY .~ r^.rectY + r^.rectH + 1) . (velocity._2 .~ 2) $ p, False)
        else checkCollisionV rs origY p

moveObjs :: [P.Platform] -> (Player,Bool) -> (Player,Bool)
moveObjs ((P.MoveablePlatform _ _ r (dx',dy') _ fwd):rs) (p, b)
    | r `collides` (rectY +~ 3 $ p^.bounding) = ((bounding.rectX +~ dx) . (velocity._2 .~ dy) $ p,True)
    | otherwise = moveObjs rs (p,b)
        where
            dx = if fwd then dx' else dx'*(-1)
            dy = if fwd then dy' else dy'*(-1)
moveObjs _ p = p
