module ChessExample.System.Mixer where

import Apecs.Effectful                    -- ECS World helpers
import Effectful                --(Eff, (:>))

import Control.Monad (void)

import ChessExample.System.World     (World)
import ChessExample.Component.Audio  (MoveSfx (..))
import UnifiedAudio.Effectful        qualified as UA

emitMoveSfx
  :: (ECS World :> es) => Eff es ()
emitMoveSfx = newEntity_ MoveSfx


audioSystem :: ( ECS World :> es, UA.Audio s :> es) => Eff es ()
audioSystem = do
  cmapM_ $ \(e, MoveSfx) -> do
    clip <- UA.load "assets/sfx/move.wav"
    _    <- UA.play clip
    destroy @MoveSfx e