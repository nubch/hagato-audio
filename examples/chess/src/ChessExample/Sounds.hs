{-# Language DataKinds #-}
module ChessExample.Sounds where

import Effectful ( (:>), Eff )
import UnifiedAudio.Effectful qualified as UA

data Sounds s = Sounds
  { moveSound    :: s 'UA.Loaded
  , knightMove   :: s 'UA.Loaded
  , victorySound :: s 'UA.Loaded
  , music        :: s 'UA.Loaded
  }

initSounds :: (UA.Audio s :> es) => Eff es (Sounds s)
initSounds = do
  move          <- UA.loadFile "examples/chess/assets/sounds/playPiece.wav" UA.Mono
  knightMove    <- UA.loadFile "examples/chess/assets/sounds/horseMove.wav" UA.Mono
  victory       <- UA.loadFile "examples/chess/assets/sounds/victory.wav"   UA.Stereo
  music         <- UA.loadFile "examples/chess/assets/sounds/music.wav"     UA.Stereo
  return $ Sounds {
    moveSound    = move,
    knightMove   = knightMove,
    victorySound = victory,
    music        = music
  }