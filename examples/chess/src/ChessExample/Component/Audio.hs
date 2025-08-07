{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module ChessExample.Component.Audio where

import Apecs.Effectful (Component(..), Global, Map)
import UnifiedAudio.Effectful qualified as UA

data Sound = Move | Select | Capture | Win
  deriving (Eq, Show)

data Request = Start | Stop | Pause | Resume
  deriving (Eq, Show)

data SoundRequest = SoundRequest {
    sound   :: Sound,
    request :: Request
}

instance Component SoundRequest
    where type Storage SoundRequest = Map SoundRequest

--instance Component CaptureSfx
    --where type Storage CaptureSfx = Map CaptureSfx

--instance Component SelectSfx
    --where type Storage SelectSfx = Map SelectSfx

--instance Component WinSfx
    --where type Storage WinSfx = Map WinSfx