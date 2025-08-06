{-# LANGUAGE TypeApplications #-}
module ChessExample.System.Mixer where

import Apecs.Effectful                    -- ECS World helpers
import Effectful                --(Eff, (:>))

import Control.Monad (void)

import ChessExample.System.World     (World)
import ChessExample.Component.Audio  
import UnifiedAudio.Effectful        qualified as UA

emitMoveSfx :: (ECS World :> es) => Eff es ()
emitMoveSfx = newEntity_ MoveSfx

emitCaptureSfx :: (ECS World :> es) => Eff es ()
emitCaptureSfx = newEntity_ CaptureSfx

emitWinSfx :: (ECS World :> es) => Eff es ()
emitWinSfx = newEntity_ WinSfx

audioSystem :: (ECS World :> es, UA.Audio s :> es) => Eff es ()
audioSystem = do
  cmapM $ \(MoveSfx) -> do
    clip <- UA.load "assets/sfx/move.wav"
    _    <- UA.play clip
    return $ Not @(MoveSfx)