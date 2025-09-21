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
  move          <- UA.loadFile "examples/chess/assets/sounds/playPiece.wav" UA.Stereo
  knightMove    <- UA.loadFile "examples/chess/assets/sounds/horseMove.wav" UA.Stereo
  select        <- UA.loadFile "examples/chess/assets/sounds/playPiece.wav" UA.Stereo
  capture       <- UA.loadFile "examples/chess/assets/sounds/playPiece.wav" UA.Stereo
  win           <- UA.loadFile "examples/chess/assets/sounds/playPiece.wav" UA.Stereo
  music         <- UA.loadFile "examples/chess/assets/sounds/music.mp3"     UA.Stereo
  return $ Sounds {
    moveSound    = move,
    knightMove   = knightMove,
    selectSound  = select,
    captureSound = capture,
    winSound     = win,
    music        = music
  }