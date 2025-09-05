{-# LANGUAGE TypeApplications #-}
module ChessExample.System.Mixer where

import Apecs.Effectful                    -- ECS (World s) helpers
import Effectful                --(Eff, (:>))

import Control.Monad (void, unless)
import Data.Typeable (Typeable)
import ChessExample.System.World     (World)
import ChessExample.Component.Audio  
import UnifiedAudio.Effectful        qualified as UA
import ChessExample.Sounds
import UnifiedAudio.Effectful (Status(Playing))

playSound :: ECS (World s) :> es => Sound -> UA.Times -> Eff es ()
playSound sound times = newEntity_ $ SoundRequest sound Start times

setMasterVolume :: ECS (World s) :> es => UA.Volume -> Eff es ()
setMasterVolume v = set global (SetMasterGain v)

muteAll :: ECS (World s) :> es => Eff es ()
muteAll = setMasterVolume (UA.mkVolume 0)

unmuteAll :: ECS (World s) :> es => Eff es ()
unmuteAll = setMasterVolume (UA.mkVolume 1)

lowerVolumeMaster :: ECS (World s) :> es => Eff es ()
lowerVolumeMaster = do
  MasterGain mg <- get global
  let v = UA.unVolume mg
      v' = max 0 (v - 0.1)
  setMasterVolume (UA.mkVolume v')

raiseVolumeMaster :: ECS (World s) :> es => Eff es ()
raiseVolumeMaster = do
  MasterGain mg <- get global
  let v = UA.unVolume mg
      v' = min 1 (v + 0.1)
  setMasterVolume (UA.mkVolume v')

toggleMute :: ECS (World s) :> es => Eff es ()
toggleMute = do
  MasterGain mg <- get global
  let v = UA.unVolume mg
  if v == 0
    then unmuteAll
    else muteAll

audioSystem :: forall s es. (Typeable s, ECS (World s) :> es, UA.Audio s :> es, IOE :> es) => Sounds s -> Eff es ()
audioSystem sounds = do
  -- Make sure master gain is initialized
  hasMG <- exists @MasterGain global
  unless hasMG $ do 
    set global (MasterGain (UA.mkVolume 1))
    liftIO $ putStrLn "Initialized Master Gain"

  -- Handle requests for playing sounds
  cmapM $ \(SoundRequest sound _ times) -> do
    MasterGain gain <- get global

    liftIO $ putStrLn $ "MASTER" ++ show gain
    let baseVolume  = UA.mkVolume 1
        finalVolume = mulVol gain baseVolume

    channel         <- UA.play (toLoadedSound sound) times
    UA.setVolume channel finalVolume
    _               <- newEntity (PlayingChannel channel, BaseVolume baseVolume)
    return $ Not @SoundRequest
  
  -- React to Master Gain changes
  cmapM $ \(SetMasterGain newGain) -> do
    set global (Not @SetMasterGain)
    liftIO $ putStrLn $ "Setting master gain to " ++ show newGain
    set global (MasterGain newGain)
    MasterGain gain <- get global
    cmapM_ $ \(PlayingChannel channel, BaseVolume base) -> do
      UA.setVolume channel (mulVol base gain)
  
  -- Remove finished channels
  cmapM $ \mapped@(PlayingChannel channel, BaseVolume _) -> do
    finished <- UA.hasFinished channel
    if finished
      then pure $ Left  (Not @(PlayingChannel s), Not @BaseVolume)
      else pure $ Right mapped

  where
    toLoadedSound s = case s of
      Move        -> sounds.moveSound
      KnightMove  -> sounds.knightMove
      Select      -> sounds.selectSound
      Capture     -> sounds.captureSound
      Music       -> sounds.music
      Win         -> sounds.winSound


mulVol :: UA.Volume -> UA.Volume -> UA.Volume
mulVol (UA.Volume a) (UA.Volume b) = UA.mkVolume (a * b)

