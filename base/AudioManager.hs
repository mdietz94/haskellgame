module HaskellGame.AudioManager where
import Graphics.UI.SDL.Mixer as SDL.Mixer

type Id = Int

--initialize::

playSound :: String -> IO Id
playSound file = SDL.Mixer.loadWAV file >>= \x -> SDL.Mixer.playChannel (-1) x 0

stopSound :: Id -> IO ()
stopSound soundID = SDL.Mixer.haltChannel (-1)

playMusic :: String -> IO ()
playMusic file = SDL.Mixer.loadMUS file >>= \x -> SDL.Mixer.playMusic x (-1)

stopMusic :: IO ()
stopMusic = SDL.Mixer.haltMusic

--update :: 
