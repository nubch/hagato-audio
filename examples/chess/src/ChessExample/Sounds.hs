{-# Language DataKinds #-}
module ChessExample.Sounds where

import Effectful
import UnifiedAudio.Effectful qualified as UA

data Sounds s = Sounds
  { moveSound    :: (s 'UA.Loaded)
  , selectSound  :: (s 'UA.Loaded)
  , captureSound :: (s 'UA.Loaded)
  , winSound     :: (s 'UA.Loaded)
}

initSounds :: (UA.Audio s :> es) => Eff es (Sounds s)
initSounds = do
  move    <- UA.load "move.wav"
  select  <- UA.load "select.wav"
  capture <- UA.load "capture.wav"
  win     <- UA.load "win.wav"
  return $ Sounds {
    moveSound    = move,
    selectSound  = select,
    captureSound = capture,
    winSound     = win
  }