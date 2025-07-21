{-# LANGUAGE DataKinds #-}

module ChessExample.Sound where

import Effectful
import UnifiedAudio.Effectful

data Sounds h = Sounds { 
    music      :: h 'Loaded,
    pickPiece  :: h 'Loaded,
    slayPiece :: h 'Loaded,
    win        :: h 'Loaded
    }

initSounds :: (Audio h :> es) => Eff es (Sounds h)
initSounds = do 
    music <- load "examples/chess/assets/sounds/music.mp3"
    pickPiece <- load "examples/chess/assets/sounds/pickPiece.mp3"
    slayPiece <- load "examples/chess/assets/sounds/slayPiece.mp3"
    win <- load "examples/chess/assets/sounds/win.mp3"
    pure $ Sounds 
        { music = music
        , pickPiece = pickPiece
        , slayPiece = slayPiece
        , win = win
        }