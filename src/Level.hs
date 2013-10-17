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
initialize 0 = Level 0 sPos (Geo.Rectangle 350 240 20 20) platforms mPlatforms (C.FixedCamera (0,0) height width) (map P.initialize sPos) 0 []
    where
        sPos = [(0,200), (100,200)]
        mPlatforms = [(Platform.moveableInitialize (0,200) (100,200) 30 10 5 (0,0,0) )]
        platforms = [(Platform.Platform (Geo.Rectangle 0 300 400 30) (0,0,0)),(Platform.Platform (Geo.Rectangle 450 290 300 30) (0,0,0)), (Platform.Platform (Geo.Rectangle 300 260 95 10) (0,0,0))]

update :: IH.KeyboardState -> Level -> (Bool,Level)
update kS lev = if IH.isDown kS SDL.SDLK_LSHIFT then (False,head (levelHistory lev)) else (not . (elem False) . (map (Geo.collides (goal nL))) $ pObs, nL)
    where
        nL = lev { players=newPlayers, levelHistory=(lev:(levelHistory lev)), curPlayer=playInd, movingObstacles=newObstacles }
        playInd = max 0 (min pI ((length (players lev)) - 1))
        pI = (curPlayer lev) + (if IH.isDown kS SDL.SDLK_q then (-1) else 0) + (if IH.isDown kS SDL.SDLK_e then 1 else 0) 
        sObs = (map Platform.bounding $ staticObstacles lev)
        pObs = (map P.bounding $ players lev)
        -- lengthy way to exclude players from hitting themselves
        newPlayers = [ P.update (if n == playInd then kS else []) (sObs ++ mObs ++ (skip n pObs)) ((players lev)!!n) | n <- [0..((length . players $ lev) - 1)] ]
        mObs = (map Platform.mBounding $ newObstacles)
        newObstacles = map Platform.update (movingObstacles lev)

draw :: SDL.Surface -> Level -> IO ()
draw screen lev = do
    -- last will force evaluation, and keep type IO ()
    sequence $ map (Platform.draw screen (camera lev)) (staticObstacles lev)
    sequence $ map (P.draw screen (camera lev)) (players lev)
    sequence $ map (Platform.draw screen (camera lev)) (movingObstacles lev)
    G.drawRect screen (Geo.rectX . goal $ lev, Geo.rectY . goal $ lev) (Geo.rectW . goal $ lev) (Geo.rectH . goal $ lev) (50,100,50)
    return ()

-- Helper function

skip :: Int -> [a] -> [a]
skip _ [] = []
skip 0 (x:xs) = xs
skip n (x:xs) = x : (skip (n-1) xs)