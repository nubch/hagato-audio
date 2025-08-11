{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module ChessExample.Component.Audio where

import Data.Kind (Type)
import Apecs.Effectful (Component(..), Global, Map)
import UnifiedAudio.Effectful qualified as UA

data Sound = Move | KnightMove | Select | Capture | Win
  deriving (Eq, Show)

data Request = Start | Stop | Pause | Resume
  deriving (Eq, Show)

data MuteAllRequest = MuteAllRequest
  deriving (Eq)

newtype PlayingChannel (s :: UA.Status -> Type) =
   PlayingChannel (s 'UA.Playing) 

data SoundRequest = SoundRequest {
    sound   :: Sound,
    request :: Request,
    times   :: UA.Times
}

instance Component SoundRequest
    where type Storage SoundRequest = Map SoundRequest

instance Component MuteAllRequest
    where type Storage MuteAllRequest = Map MuteAllRequest

instance Component (PlayingChannel s)
    where type Storage (PlayingChannel s) = Map (PlayingChannel s)