{-# LANGUAGE TypeApplications #-}
module ChessExample.System.Mixer where

import Apecs.Effectful                    -- ECS World helpers
import Effectful                --(Eff, (:>))

import Control.Monad (void)

import ChessExample.System.World     (World)
import ChessExample.Component.Audio  
import UnifiedAudio.Effectful        qualified as UA
import ChessExample.Sounds

playSound :: (ECS World :> es) => Sound -> Eff es ()
playSound sound = newEntity_ $ SoundRequest sound Start 

audioSystem :: (ECS World :> es, UA.Audio s :> es) => Sounds s -> Eff es ()
audioSystem sounds = do
  cmapM $ \(SoundRequest sound request) -> do
    _    <- UA.play (toLoadedSound sound)
    return $ Not @(SoundRequest)
  where
    toLoadedSound Move    = sounds.moveSound
    toLoadedSound Select  = sounds.selectSound
    toLoadedSound Capture = sounds.captureSound
    toLoadedSound Win     = sounds.winSound

