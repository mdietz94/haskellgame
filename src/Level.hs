{-# LANGUAGE FlexibleContexts #-}
module Level where

import Control.Monad
import Control.Monad.State
import qualified Player as P
import qualified Platform as Platform
import qualified Base.InputHandler as IH
import qualified Base.AudioManager as AM
import qualified Base.Camera as C
import qualified Base.Geometry as Geo
import qualified Graphics.UI.SDL as SDL
import Config
import Timer

data Level = Level {
    lId :: Int,
    startPos :: (Int,Int),
    goal :: Geo.Shape,
    staticObstacles :: [Platform.Platform],
    camera :: C.FixedCamera,
    player :: P.Player,
    levelHistory :: [Level]
}

initialize :: Int -> Level
initialize 0 = Level 0 sPos (Geo.Point 300 200) platforms (C.FixedCamera (0,0) height width) (P.initialize sPos) []
    where
        sPos = (0,300) :: (Int,Int)
        platforms = [(Platform.Platform (Geo.Rectangle 0 300 400 30) (0,0,0))] :: [Platform.Platform]

update :: IH.KeyboardState -> Level -> (Bool,Level)
update kS lev = if IH.isDown kS SDL.SDLK_LSHIFT then (False,head (levelHistory lev)) else ((goal nL) `Geo.collides` (P.bounding nP), nL)
    where
        nL = lev { player=nP, levelHistory=(lev:(levelHistory lev)) }
        nP = P.update kS (map Platform.bounding $ staticObstacles lev) (player lev)


draw :: SDL.Surface -> Level -> IO ()
draw screen lev = do
    -- last will force evaluation, and keep type IO ()
    last $ map (Platform.draw screen (camera lev)) (staticObstacles lev)
    P.draw screen (camera lev) (player lev)
    return ()
