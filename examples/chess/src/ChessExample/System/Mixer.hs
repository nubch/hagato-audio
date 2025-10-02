{-# LANGUAGE TypeApplications #-}
module ChessExample.System.Mixer where

import Apecs.Effectful                    -- ECS (World s) helpers
import Effectful                --(Eff, (:>))

import Control.Monad (unless)
import Data.Typeable (Typeable)
import ChessExample.System.World     (World)
import ChessExample.Component.Audio  
import UnifiedAudio.Effectful        qualified as UA
import ChessExample.Sounds

playSound :: ECS (World s) :> es => Sound -> UA.Times -> GroupTag -> Eff es ()
playSound sound times grp = newEntity_ $ SoundRequest sound Start times grp

playMusic sound = 
  

raiseGroupVolume :: (Typeable s, ECS (World s) :> es, UA.Audio s :> es) => GroupTag -> Eff es ()
raiseGroupVolume grp = do 
  group <- resolveGroup grp
  baseVol <- UA.unVolume <$> UA.getGroupVolume group
  UA.setGroupVolume group (UA.mkVolume $ baseVol + 0.1)

lowerGroupVolume :: (Typeable s, ECS (World s) :> es, UA.Audio s :> es) => GroupTag -> Eff es ()
lowerGroupVolume grp = do 
  group <- resolveGroup grp
  baseVol <- UA.unVolume <$> UA.getGroupVolume group
  UA.setGroupVolume group (UA.mkVolume $ baseVol - 0.1)

-- Panning: -1 = left, +1 = right
adjustGroupPanning :: (Typeable s, ECS (World s) :> es, UA.Audio s :> es) => GroupTag -> Float -> Eff es ()
adjustGroupPanning grp delta = do
  group <- resolveGroup grp
  basePan <- UA.unPanning <$> UA.getGroupPanning group
  UA.setGroupPanning group (UA.mkPanning $ basePan + delta)

-- Nudge helpers
nudgeLeft :: (Typeable s, ECS (World s) :> es, UA.Audio s :> es) => GroupTag -> Eff es ()
nudgeLeft grp = adjustGroupPanning grp (-0.1)

nudgeRight :: (Typeable s, ECS (World s) :> es, UA.Audio s :> es) => GroupTag -> Eff es ()
nudgeRight grp = adjustGroupPanning grp 0.1

audioSystem :: forall s es. (Typeable s, ECS (World s) :> es, UA.Audio s :> es, IOE :> es) => Sounds s -> Eff es ()
audioSystem sounds = do
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
    group <- resolveGroup grp

    channel <- UA.play (toLoadedSound sound) times
    UA.addToGroup group channel
    newEntity_ (PlayingChannel channel)

    return $ Not @SoundRequest
  
  -- Remove finished channels
  cmapM $ \mapped@(PlayingChannel channel) -> do
    finished <- UA.hasFinished channel
    if finished
      then do 
        _ <- UA.stop channel 
        pure $ Left  (Not @(PlayingChannel s))
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

-- Toggle pause for a group
togglePauseGroup :: (Typeable s, ECS (World s) :> es, UA.Audio s :> es) => GroupTag -> Eff es ()
togglePauseGroup grpTag = do
  group <- resolveGroup grpTag
  isPaused <- UA.isGroupPaused group
  if isPaused
    then do UA.resumeGroup group
    else do UA.pauseGroup group

-- Stop all channels in a group
stopGroup :: (Typeable s, ECS (World s) :> es, UA.Audio s :> es) => GroupTag -> Eff es ()
stopGroup grpTag = resolveGroup grpTag >>= UA.stopGroup

resolveGroup
  :: ( Typeable s, ECS (World s) :> es ) => GroupTag -> Eff es (UA.Group s)
resolveGroup SFXgrp = (\(SFXGroup g)   -> g) <$> get global
resolveGroup Musicgrp = (\(MusicGroup g) -> g) <$> get global