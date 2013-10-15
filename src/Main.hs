{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Word
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import qualified Player as P
import qualified Platform as Platform
import qualified Base.GraphicsManager as G
import qualified Base.InputHandler as IH
import qualified Base.AudioManager as AM
import qualified Base.Camera as C
import qualified Base.Geometry as Geo
import qualified Graphics.UI.SDL as SDL
import Timer

screenWidth  = 640
screenHeight = 480
screenBpp    = 32


data AppData = AppData {
    camera :: C.FixedCamera,
    keyboardState :: IH.KeyboardState,
    player :: P.Player,
    fps :: Timer
}

data AppConfig = AppConfig {
    screen    :: SDL.Surface
}

type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState

getFPS :: MonadState AppData m => m Timer
getFPS = liftM fps get

putFPS :: MonadState AppData m => Timer -> m ()
putFPS t = modify $ \s -> s { fps = t }

putKeyboardState :: MonadState AppData m => IH.KeyboardState -> m ()
putKeyboardState t = modify $ \s -> s { keyboardState = t }

getKeyboardState :: MonadState AppData m => m IH.KeyboardState
getKeyboardState = liftM keyboardState get

getPlayer :: MonadState AppData m => m P.Player
getPlayer = liftM player get

getScreen :: MonadReader AppConfig m => m SDL.Surface
getScreen = liftM screen ask

putPlayer :: MonadState AppData m => P.Player -> m ()
putPlayer t = modify $ \s -> s { player = t }

getCamera :: MonadState AppData m => m C.FixedCamera
getCamera = liftM camera get

modifyFPSM :: MonadState AppData m => (Timer -> m Timer) -> m ()
modifyFPSM act = getFPS >>= act >>= putFPS

initEnv :: IO (AppConfig, AppData)
initEnv = do    
    screen <- G.initialize 640 480 "Best Game Ever"
    (_,keys) <- IH.initialize
    let player = P.initialize (0,0)
    let camera = C.FixedCamera (0,0) 1000 1000
    fps <- start defaultTimer
    return (AppConfig screen, AppData camera keys player fps) 

loop :: AppEnv ()
loop = do
    modifyFPSM $ liftIO . start
    keyboardState <- getKeyboardState
    (quit,keyState) <- liftIO $ IH.update keyboardState
    putKeyboardState keyState
    --liftIO $ putStrLn . show $ IH.isDown keyState SDL.SDLK_LEFT
    fps <- getFPS

    player <- getPlayer
    camera <- getCamera
    let newPlayer = P.update keyState [(Geo.Rectangle (-20) 20 100 20), (Geo.Rectangle 100 60 100 20)] player
    putPlayer newPlayer

    putKeyboardState $ IH.putLastKeyboardState keyState
    screen    <- getScreen
    liftIO $ do
        G.begin screen
        Platform.draw screen (Platform.Platform (Geo.Rectangle 0 52 55 20) (0xff,0x00,0xff)) camera
        Platform.draw screen (Platform.Platform (Geo.Rectangle 110 80 65 20) (0xff,0x00,0xff)) camera
        --G.drawRect screen (0,0) 640 480 (0x00,0x00,0x00)
        P.draw screen player camera
        G.end screen
        ticks <- getTimerTicks fps
        when (ticks < secsPerFrame) $ do
            SDL.delay $ secsPerFrame - ticks

    unless quit loop
 where
    framesPerSecond = 20
    secsPerFrame    = 1000 `div` framesPerSecond

runLoop :: AppConfig -> AppData -> IO ()
runLoop = evalStateT . runReaderT loop

main = SDL.withInit [SDL.InitEverything] $ do -- withInit calls quit for us.
    (env, state) <- initEnv
    runLoop env state
