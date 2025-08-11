{-# Language DataKinds #-}
module ChessExample.Sounds where

import Effectful
import UnifiedAudio.Effectful qualified as UA

data Sounds s = Sounds
  { moveSound    :: (s 'UA.Loaded)
  , knightMove   :: (s 'UA.Loaded)
  , selectSound  :: (s 'UA.Loaded)
  , captureSound :: (s 'UA.Loaded)
  , winSound     :: (s 'UA.Loaded)
  , music        :: (s 'UA.Loaded)
  }

initSounds :: (UA.Audio s :> es) => Eff es (Sounds s)
initSounds = do
  move          <- UA.load "examples/chess/assets/sounds/playPiece.wav"
  knightMove    <- UA.load "examples/chess/assets/sounds/horseMove.wav"
  select        <- UA.load "examples/chess/assets/sounds/playPiece.wav"
  capture       <- UA.load "examples/chess/assets/sounds/playPiece.wav"
  win           <- UA.load "examples/chess/assets/sounds/playPiece.wav"
  music         <- UA.load "examples/chess/assets/sounds/music.mp3"
  return $ Sounds {
    moveSound    = move,
    knightMove   = knightMove,
    selectSound  = select,
    captureSound = capture,
    winSound     = win,
    music        = music
  }