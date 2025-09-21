{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module ChessExample.Component.Audio where

import Data.Kind (Type)
import Apecs.Effectful (Component(..), Map, Unique)
import UnifiedAudio.Effectful qualified as UA

data Sound = Move | KnightMove | Select | Capture | Win | Music
  deriving (Eq, Show)

data GroupTag = SFXgrp | Musicgrp
  deriving (Eq, Show)

data Request = Start | Stop | Pause | Resume
  deriving (Eq, Show)

newtype MasterGain = MasterGain UA.Volume

newtype SetMasterGain = SetMasterGain UA.Volume

newtype SFXGroup s = SFXGroup (UA.Group (s :: UA.Status -> Type))

newtype MusicGroup s = MusicGroup (UA.Group (s :: UA.Status -> Type))

newtype PlayingChannel (s :: UA.Status -> Type) =
   PlayingChannel (s 'UA.Playing) 

newtype BaseVolume = BaseVolume UA.Volume

data SoundRequest = SoundRequest {
    sound   :: Sound,
    request :: Request,
    times   :: UA.Times,
    group   :: GroupTag
}

instance Component SoundRequest
    where type Storage SoundRequest = Map SoundRequest

instance Component MasterGain 
    where type Storage MasterGain = Unique MasterGain

instance Component (PlayingChannel s)
    where type Storage (PlayingChannel s) = Map (PlayingChannel s)

instance Component SetMasterGain
    where type Storage SetMasterGain = Unique SetMasterGain

instance Component (SFXGroup s)
    where type Storage (SFXGroup s) = Unique (SFXGroup s)

instance Component (MusicGroup s)
    where type Storage (MusicGroup s) = Unique (MusicGroup s)

instance Component BaseVolume
    where type Storage BaseVolume = Map BaseVolume