{-# LANGUAGE TypeApplications #-}
module ChessExample.System.Mixer where

import Apecs.Effectful                    -- ECS World helpers
import Effectful                --(Eff, (:>))

import Control.Monad (void)

import ChessExample.System.World     (World)
import ChessExample.Component.Audio  
import UnifiedAudio.Effectful        qualified as UA
import ChessExample.Sounds

playSound :: (ECS World :> es) => Sound -> UA.Times -> Eff es ()
playSound sound times = newEntity_ $ SoundRequest sound Start times

--muteAllChannels :: (ECS World :> es) => Eff es ()
--muteAllChannels = newEntity_ MuteAllRequest

audioSystem :: (ECS World :> es, UA.Audio s :> es) => Sounds s -> Eff es ()
audioSystem sounds = do
  cmapM $ \(SoundRequest sound request times) -> do
    channel    <- UA.play (toLoadedSound sound) times
    return $ Not @(SoundRequest)
  where
    toLoadedSound Move    = sounds.moveSound
    toLoadedSound KnightMove = sounds.knightMove
    toLoadedSound Select  = sounds.selectSound
    toLoadedSound Capture = sounds.captureSound
    toLoadedSound Win     = sounds.winSound

