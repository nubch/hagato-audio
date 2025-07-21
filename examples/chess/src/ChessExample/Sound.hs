{-# LANGUAGE DataKinds #-}

module ChessExample.Sound where

import Effectful
import UnifiedAudio.Effectful

data Sounds h = Sounds { 
    music      :: h 'Loaded,
    playPiece  :: h 'Loaded
    }

initSounds :: (Audio h :> es) => Eff es (Sounds h)
initSounds = do 
    music <- load "examples/chess/assets/sounds/music.mp3"
    playPiece <- load "examples/chess/assets/sounds/playPiece.wav"
    pure $ Sounds 
        { music = music
        , playPiece = playPiece
        }