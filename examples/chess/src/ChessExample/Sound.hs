{-# LANGUAGE DataKinds #-}

module ChessExample.Sound where

import Effectful
import UnifiedAudio.Effectful

data Sounds h = Sounds { 
    music      :: h 'Loaded,
    pickPiece  :: h 'Loaded,
    placePiece :: h 'Loaded,
    undoMove   :: h 'Loaded,
    win        :: h 'Loaded,
    lose       :: h 'Loaded
    }

initSounds :: (Audio h :> es) => Eff es (Sounds h)
initSounds = do 
    music <- load "../assets/music.mp3"
    pickPiece <- load "../assets/pickPiece.mp3"
    placePiece <- load "../assets/placePiece.mp3"
    undoMove <- load "../assets/undoMove.mp3"
    win <- load "../assets/win.mp3"
    lose <- load "../assets/lose.mp3"
    pure $ Sounds 
        { music = music
        , pickPiece = pickPiece
        , placePiece = placePiece
        , undoMove = undoMove
        , win = win
        , lose = lose
        }