{-# LANGUAGE TypeFamilies #-}
module ChessExample.Component.Audio
  ( -- * One-tick SFX events
    MoveEvt(..)
  , CaptureEvt(..)
  , WinEvt(..)
    -- * Background music control
  , MusicMode(..)
  , MusicVoice(..)
  , NowPlaying(..)
  ) where

import Apecs.Effectful (Component(..), Global, Map)

-- | Fire-and-forget sound effects (consumed in the same tick).
data MoveEvt    = MoveEvt
data CaptureEvt = CaptureEvt
data WinEvt     = WinEvt

-- | Which background track we want.
data MusicMode = Menu | Playing | Won
  deriving (Eq, Show)

-- | Handle/voice returned by your audio backend for a looping track.
-- Adapt the payload if your backend uses a different handle type.
newtype MusicVoice = MusicVoice Int
  deriving (Eq, Show)

-- | What is currently playing (mode + its handle), or nothing yet.
newtype NowPlaying = NowPlaying (Maybe (MusicMode, MusicVoice))
  deriving (Eq, Show)

-- Storage choices -------------------------------------------------------------

instance Component MoveEvt    where type Storage MoveEvt    = Map MoveEvt
instance Component CaptureEvt where type Storage CaptureEvt = Map CaptureEvt
instance Component WinEvt     where type Storage WinEvt     = Map WinEvt

instance Component MusicMode  where type Storage MusicMode  = Global MusicMode
instance Component NowPlaying where type Storage NowPlaying = Global NowPlaying