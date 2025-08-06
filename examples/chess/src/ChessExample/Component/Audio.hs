{-# LANGUAGE TypeFamilies #-}
module ChessExample.Component.Audio where

import Apecs.Effectful (Component(..), Global, Map)

data MoveSfx = MoveSfx

data SelectSfx = SelectSfx

data CaptureSfx = CaptureSfx

data WinSfx = WinSfx

instance Component MoveSfx
    where type Storage MoveSfx = Map MoveSfx

instance Component CaptureSfx
    where type Storage CaptureSfx = Map CaptureSfx

instance Component SelectSfx
    where type Storage SelectSfx = Map SelectSfx

instance Component WinSfx
    where type Storage WinSfx = Map WinSfx