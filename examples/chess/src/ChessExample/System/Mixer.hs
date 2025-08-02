{-# LANGUAGE DataKinds, TypeApplications #-}
module ChessExample.System.Mixer
  ( audioSystem          -- run this each frame
  , emitMoveSfx          -- helpers you call from gameplay code
  , emitCaptureSfx
  , emitWinSfx
  , setMusic             -- set desired background track
  ) where

import Apecs.Effectful                    -- ECS World helpers
import Control.Monad (void, when)
import Effectful                (Eff, (:>))

import ChessExample.System.World        (World)
import           ChessExample.Component.Audio        -- MoveEvt, CaptureEvt, WinEvt, MusicMode…
--------------------------------------------------------------------------------
-- Small emit / setter helpers -------------------------------------------------

emitMoveSfx
  :: (ECS World :> es, Set World MoveEvt, Get World EntityCounter)
  => Eff es ()
emitMoveSfx = void (newEntity MoveEvt)

emitCaptureSfx
  :: (ECS World :> es, Set World CaptureEvt, Get World EntityCounter)
  => Eff es ()
emitCaptureSfx = void (newEntity CaptureEvt)

emitWinSfx
  :: (ECS World :> es, Set World WinEvt, Get World EntityCounter)
  => Eff es ()
emitWinSfx = void (newEntity WinEvt)

setMusic
  :: (ECS World :> es, Set World MusicMode)
  => MusicMode -> Eff es ()
setMusic = set global

-- | Run once per frame *after* gameplay code has emitted events
--   and (optionally) changed MusicMode.
audioSystem
  :: forall s es
   . ( ECS World :> es
     -- SFX events
     , Members World (Entity, MoveEvt),    Destroy World MoveEvt
     , Members World (Entity, CaptureEvt), Destroy World CaptureEvt
     , Members World (Entity, WinEvt),     Destroy World WinEvt
     -- Music globals
     , Get World MusicMode
     , Get World NowPlaying
     , Set World NowPlaying
     -- Your backend effect
     , Audio.Audio s :> es
     , IOE :> es
     )
  => Audio.Sounds s      -- ^ bundle of loaded sounds / music tracks
  -> Eff es ()
audioSystem sounds = do
  ------------------------------------------------------------------------------
  -- 1.  One‑shot SFX  ----------------------------------------------------------
  ------------------------------------------------------------------------------
  cmapM_ \(e, MoveEvt) -> do
    _ <- Audio.play (Audio.moveSfx sounds)       -- play “move”
    destroy @MoveEvt e

  cmapM_ \(e, CaptureEvt) -> do
    _ <- Audio.play (Audio.captureSfx sounds)    -- play “capture”
    destroy @CaptureEvt e

  cmapM_ \(e, WinEvt) -> do
    _ <- Audio.play (Audio.winSfx sounds)        -- play “win”
    destroy @WinEvt e
    -- Optional: auto‑switch to "Won" music when win SFX fires
    set @World @MusicMode global Won

  ------------------------------------------------------------------------------
  -- 2.  Exactly‑one background track  -----------------------------------------
  ------------------------------------------------------------------------------
  desired        <- get @World @MusicMode  global
  NowPlaying cur <- get @World @NowPlaying global

  let needChange = case cur of
                     Just (mode, _) -> mode /= desired
                     Nothing        -> True

  when needChange $ do
    -- Stop any currently playing track
    case cur of
      Just (_, voice) -> Audio.stopVoice voice
      _               -> pure ()

    -- Start the requested track, receive new handle
    let track = case desired of
                  Menu    -> Audio.menuMusic sounds
                  Playing -> Audio.gameMusic sounds
                  Won     -> Audio.wonMusic sounds
    newVoice <- Audio.startLoop track

    -- Remember what’s now playing
    set global (NowPlaying (Just (desired, newVoice)))