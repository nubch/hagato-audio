{-# LANGUAGE TypeApplications #-}
module ChessExample.System.Mixer where

import Apecs.Effectful                    -- ECS (World s) helpers
import Effectful                --(Eff, (:>))

import Control.Monad (void)
import Data.Typeable (Typeable)
import ChessExample.System.World     (World)
import ChessExample.Component.Audio  
import UnifiedAudio.Effectful        qualified as UA
import ChessExample.Sounds

playSound :: ECS (World s) :> es => Sound -> UA.Times -> Eff es ()
playSound sound times = newEntity_ $ SoundRequest sound Start times

muteAllChannels :: ECS (World s) :> es => Eff es ()
muteAllChannels = set global MuteAllRequest

audioSystem :: forall s es. (Typeable s, ECS (World s) :> es, UA.Audio s :> es, IOE :> es) => Sounds s -> Eff es ()
audioSystem sounds = do
  cmapM $ \(SoundRequest sound request times) -> do
    channel    <- UA.play (toLoadedSound sound) UA.Forever
    e <- newEntity $ PlayingChannel channel
    UA.onFinished channel (\channel -> set e (Not @(PlayingChannel s))) 
    return $ Not @SoundRequest
  cmapM_ $ \(_ :: MuteAllRequest) -> do
    cmapM_ $ \(PlayingChannel channel) -> do
      liftIO $ putStrLn $ "Muting channel"
      void $ UA.mute channel
    set global (Not @MuteAllRequest)
  where
    toLoadedSound s = case s of
      Move        -> sounds.moveSound
      KnightMove  -> sounds.knightMove
      Select      -> sounds.selectSound
      Capture     -> sounds.captureSound
      Win         -> sounds.winSound

