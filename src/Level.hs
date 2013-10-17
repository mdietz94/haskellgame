{-# LANGUAGE FlexibleContexts #-}
module Level where

import Control.Monad
import Control.Monad.State
import qualified Player as P
import qualified Platform as Platform
import qualified Base.InputHandler as IH
import qualified Base.AudioManager as AM
import qualified Base.GraphicsManager as G
import qualified Base.Camera as C
import qualified Base.Geometry as Geo
import qualified Graphics.UI.SDL as SDL
import Config
import Timer

{-

We should use these structures to avoid overhead when generating
level history.

data LevelConfig = LevelConfig {
    lId :: Int,
    startPos :: [(Int,Int)],
    goal :: Geo.Shape,
    staticObstacles :: [Platform.Platform],
}

data LevelData = LevelData {
    movingObstacles :: [Platform.MoveablePlatform],
    camera :: C.FixedCamera,
    players :: [P.Player],
    curPlayer :: Int,
    levelHistory :: [LevelData]
}

-}

data Level = Level {
    lId :: Int,
    startPos :: [(Int,Int)],
    goal :: Geo.Shape,
    staticObstacles :: [Platform.Platform],
    movingObstacles :: [Platform.Platform],
    camera :: C.FixedCamera,
    players :: [P.Player],
    curPlayer :: Int,
    levelHistory :: [Level]
}

initialize :: Int -> Level
initialize 0 = Level 0 sPos (Geo.Point 300 220) platforms mPlatforms (C.FixedCamera (0,0) height width) (map P.initialize sPos) 0 []
    where
        sPos = [(0,200), (100,200)]
        mPlatforms = [(Platform.moveableInitialize (0,100) (100,100) 30 10 5 (0,0,0) )]
        platforms = [(Platform.Platform (Geo.Rectangle 0 300 400 30) (0,0,0)),(Platform.Platform (Geo.Rectangle 450 290 300 30) (0,0,0)), (Platform.Platform (Geo.Rectangle 300 260 95 10) (0,0,0))]

{-

We need to differentiate between objects, since right now objects of the same
type cannot collide because otherwise they will always detect collisions against
themselves.  We could just check to see if the objects are equivalent, though
working around the indices is probably a better idea.

-}

update :: IH.KeyboardState -> Level -> (Bool,Level)
update kS lev = if IH.isDown kS SDL.SDLK_LSHIFT then (False,head (levelHistory lev)) else ((goal nL) `Geo.collides` (P.bounding nP), nL)
    where
        nL = lev { players=pls2, levelHistory=(lev:(levelHistory lev)), curPlayer=playInd, movingObstacles=newObstacles }
        newObstacles = map (Platform.update $ sObs ++ pObs) (movingObstacles lev)
        nP = P.update kS (mObs ++ sObs) (pls!!playInd)
        pls2 = [ if p /= playInd then P.update [] (mObs ++ sObs) (pls!!p) else nP | p <- [0..((length pls) - 1)]]
        pls = players lev
        playInd = max 0 (min pI ((length (players lev)) - 1))
        pI = (curPlayer lev) + (if IH.isDown kS SDL.SDLK_q then (-1) else 0) + (if IH.isDown kS SDL.SDLK_e then 1 else 0) 
        sObs = (map Platform.bounding $ staticObstacles lev)
        mObs = (map Platform.mBounding $ movingObstacles lev)
        pObs = (map P.bounding $ players lev)

draw :: SDL.Surface -> Level -> IO ()
draw screen lev = do
    -- last will force evaluation, and keep type IO ()
    sequence $ map (Platform.draw screen (camera lev)) (staticObstacles lev)
    sequence $ map (P.draw screen (camera lev)) (players lev)
    sequence $ map (Platform.draw screen (camera lev)) (movingObstacles lev)
    G.drawRect screen ((\p -> (Geo.ptX p) - 5) . goal $ lev, (\p -> (Geo.ptY p) - 5) . goal $ lev) 10 10 (50,100,50)
    return ()
