{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module ChessExample.Component.Audio where

import Data.Kind (Type)
import Apecs.Effectful (Component(..), Map, Unique)
import UnifiedAudio.Effectful qualified as UA

data Sound = Move | KnightMove | Victory | Music
  deriving (Eq, Show)

data GroupTag = SFXgrp | Musicgrp
  deriving (Eq, Show)

newtype SFXGroup s = SFXGroup (UA.Group (s :: UA.Status -> Type))

newtype MusicGroup s = MusicGroup (UA.Group (s :: UA.Status -> Type))

newtype PlayingChannel (s :: UA.Status -> Type) =
   PlayingChannel (s 'UA.Playing) 

data SoundRequest = SoundRequest {
    sound   :: Sound,
    loopMode  :: UA.LoopMode,
    group   :: GroupTag
}

instance Component SoundRequest
    where type Storage SoundRequest = Map SoundRequest

instance Component (PlayingChannel s)
    where type Storage (PlayingChannel s) = Map (PlayingChannel s)

instance Component (SFXGroup s)
    where type Storage (SFXGroup s) = Unique (SFXGroup s)

instance Component (MusicGroup s)
    where type Storage (MusicGroup s) = Unique (MusicGroup s)