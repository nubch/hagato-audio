{-# LANGUAGE TypeApplications #-}
module ChessExample.System.Mixer where

import Apecs.Effectful                    -- ECS (World s) helpers
import Effectful                --(Eff, (:>))

import Control.Monad (unless)
import Data.Typeable (Typeable)
import ChessExample.System.World     (World (baseVolume))
import ChessExample.Component.Audio  
import UnifiedAudio.Effectful        qualified as UA
import ChessExample.Sounds
import UnifiedAudio.Effectful (mkPanning)

playSound :: ECS (World s) :> es => Sound -> UA.Times -> GroupTag -> Eff es ()
playSound sound times grp = newEntity_ $ SoundRequest sound Start times grp

setMasterVolume :: ECS (World s) :> es => UA.Volume -> Eff es ()
setMasterVolume v = set global (SetMasterGain v)

muteAll :: ECS (World s) :> es => Eff es ()
muteAll = setMasterVolume (UA.mkVolume 0)

unmuteAll :: ECS (World s) :> es => Eff es ()
unmuteAll = setMasterVolume (UA.mkVolume 1)

raiseGroupVolume :: (Typeable s, ECS (World s) :> es, UA.Audio s :> es) => GroupTag -> Eff es ()
raiseGroupVolume grp = do 
  group <- toGroup grp
  baseVol <- UA.unVolume <$> UA.getGroupVolume group
  UA.setGroupVolume group (UA.mkVolume $ baseVol + 0.1)
  where 
    toGroup SFXgrp = do
      SFXGroup sfx <- get global
      pure sfx
    toGroup Musicgrp = do 
      MusicGroup mus <- get global
      pure mus

lowerGroupVolume :: (Typeable s, ECS (World s) :> es, UA.Audio s :> es) => GroupTag -> Eff es ()
lowerGroupVolume grp = do 
  group <- toGroup grp
  baseVol <- UA.unVolume <$> UA.getGroupVolume group
  UA.setGroupVolume group (UA.mkVolume $ baseVol - 0.1)
  where 
    toGroup SFXgrp = do
      SFXGroup sfx <- get global
      pure sfx
    toGroup Musicgrp = do 
      MusicGroup mus <- get global
      pure mus

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

toggleMute :: (ECS (World s) :> es, UA.Audio s :> es) => Eff es ()
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

  hasSFX <- exists @(SFXGroup s) global
  unless hasSFX $ do 
    sfx <- UA.makeGroup
    set global (SFXGroup sfx )

  hasMus <- exists @(MusicGroup s) global
  unless hasMus $ do 
    mus <- UA.makeGroup
    set global (MusicGroup mus )

  -- Handle requests for playing sounds
  cmapM $ \(SoundRequest sound _ times grp) -> do
    MasterGain gain <- get global
    SFXGroup sfxgroup <- get global
    MusicGroup musicgroup <- get global

    let group = if grp == SFXgrp then sfxgroup else musicgroup


    liftIO $ putStrLn $ "MASTER" ++ show gain

    channel         <- UA.play (toLoadedSound sound) times
    UA.addToGroup group channel
    _               <- newEntity (PlayingChannel channel)


    return $ Not @SoundRequest
  
  -- React to Master Gain changes
  --cmapM $ \(SetMasterGain newGain) -> do
    --set global (Not @SetMasterGain)
    --liftIO $ putStrLn $ "Setting master gain to " ++ show newGain
    --set global (MasterGain newGain)
    --MasterGain gain <- get global
    --cmapM_ $ \(PlayingChannel channel) -> do
      --liftIO $ putStrLn $ "Updating channel volume"
      --UA.setVolume channel (mulVol base gain)
  
  -- Remove finished channels
  cmapM $ \mapped@(PlayingChannel channel) -> do
    finished <- UA.hasFinished channel
    if finished
      then do 
        UA.stop channel 
        pure $ Left  (Not @(PlayingChannel s), Not @BaseVolume)
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

