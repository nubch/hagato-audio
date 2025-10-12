{-# LANGUAGE TypeApplications #-}

module ChessExample.System.Mixer
  ( playMusic,
    playSFX,
    raiseGroupVolume,
    lowerGroupVolume,
    nudgeLeft,
    nudgeRight,
    togglePauseGroup,
    stopGroup,
    audioSystem,
  )
where

import Apecs.Effectful (ECS, Not (Not), cfold, cmapM, newEntity_)
import ChessExample.Component.Audio
  ( GroupTag (..),
    MusicGroup (..),
    PlayingChannel (..),
    SFXGroup (..),
    Sound (..),
    SoundRequest (SoundRequest),
  )
import ChessExample.Sounds
  ( Sounds
      ( knightMove,
        moveSound,
        music,
        victorySound
      ),
  )
import ChessExample.System.World (World)
import Data.Typeable (Typeable)
import Effectful (Eff, (:>))
import UnifiedAudio.Effectful qualified as UA

type AudioCtx s es = (Typeable s, ECS (World s) :> es, UA.Audio s :> es)

-- Playing
playSound :: (ECS (World s) :> es) => Sound -> UA.LoopMode -> GroupTag -> Eff es ()
playSound sound loopMode grp = newEntity_ $ SoundRequest sound loopMode grp

playMusic :: (ECS (World s) :> es) => Sound -> Eff es ()
playMusic sound = playSound sound UA.Forever Musicgrp

playSFX :: (ECS (World s) :> es) => Sound -> Eff es ()
playSFX sound = playSound sound UA.Once SFXgrp

-- Volume Adjustment
adjustGroupVolume :: (AudioCtx s es) => GroupTag -> Float -> Eff es ()
adjustGroupVolume grp delta = do
  group <- resolveGroup grp
  baseVol <- UA.unVolume <$> UA.getGroupVolume group
  UA.setGroupVolume group (UA.mkVolume $ baseVol + delta)

raiseGroupVolume :: (AudioCtx s es) => GroupTag -> Eff es ()
raiseGroupVolume grp = adjustGroupVolume grp 0.1

lowerGroupVolume :: (AudioCtx s es) => GroupTag -> Eff es ()
lowerGroupVolume grp = adjustGroupVolume grp (-0.1)

-- Placement Adjustment
adjustGroupPlacement :: (AudioCtx s es) => GroupTag -> Float -> Eff es ()
adjustGroupPlacement grp delta = do
  group <- resolveGroup grp
  basePlacement <- UA.unPlacement <$> UA.getGroupPlacement group
  UA.setGroupPlacement group (UA.mkPlacement $ basePlacement + delta)

nudgeLeft :: (AudioCtx s es) => GroupTag -> Eff es ()
nudgeLeft grp = adjustGroupPlacement grp (-0.1)

nudgeRight :: (AudioCtx s es) => GroupTag -> Eff es ()
nudgeRight grp = adjustGroupPlacement grp 0.1

-- Group State Operations
togglePauseGroup :: (AudioCtx s es) => GroupTag -> Eff es ()
togglePauseGroup grpTag = do
  group <- resolveGroup grpTag
  isPaused <- UA.isGroupPaused group
  if isPaused
    then do UA.resumeGroup group
    else do UA.pauseGroup group

stopGroup :: (AudioCtx s es) => GroupTag -> Eff es ()
stopGroup grpTag = resolveGroup grpTag >>= UA.stopGroup

-- SYSTEM
audioSystem :: forall s es. (AudioCtx s es) => Sounds s -> Eff es ()
audioSystem sounds = do
  -- Handle requests for playing sounds
  cmapM $ \(SoundRequest sound loopMode grp) -> do
    group <- resolveGroup grp

    channel <- UA.playOnGroup (toLoadedSound sound) group loopMode
    newEntity_ (PlayingChannel channel)

    return $ Not @SoundRequest

  -- Remove finished channels
  cmapM $ \mapped@(PlayingChannel channel) -> do
    finished <- UA.hasFinished channel
    if finished
      then do
        _ <- UA.stop channel
        pure $ Left (Not @(PlayingChannel s))
      else pure $ Right mapped
  where
    toLoadedSound s = case s of
      Move       -> sounds.moveSound
      KnightMove -> sounds.knightMove
      Music      -> sounds.music
      Victory    -> sounds.victorySound

-- Helpers
createOrGetMusicGroup :: forall s es. (AudioCtx s es) => Eff es (UA.Group s)
createOrGetMusicGroup = do
  hasMusic <- cfold (\_ (mg :: MusicGroup s) -> Just mg) (Nothing :: Maybe (MusicGroup s))
  case hasMusic of
    Just (MusicGroup grp) -> pure grp
    Nothing -> do
      grp <- UA.makeGroup
      newEntity_ (MusicGroup grp)
      pure grp

createOrGetSFXGroup :: forall s es. (AudioCtx s es) => Eff es (UA.Group s)
createOrGetSFXGroup = do
  hasSfx <- cfold (\_ (mg :: SFXGroup s) -> Just mg) (Nothing :: Maybe (SFXGroup s))
  case hasSfx of
    Just (SFXGroup grp) -> pure grp
    Nothing -> do
      grp <- UA.makeGroup
      newEntity_ (SFXGroup grp)
      pure grp

resolveGroup :: (AudioCtx s es) => GroupTag -> Eff es (UA.Group s)
resolveGroup SFXgrp = createOrGetSFXGroup
resolveGroup Musicgrp = createOrGetMusicGroup