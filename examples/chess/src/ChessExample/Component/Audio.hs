{-# LANGUAGE TypeFamilies #-}
module ChessExample.Component.Audio where

import Apecs.Effectful (Component(..), Global, Map)

data MoveSfx    = MoveSfx

instance Component MoveSfx
    where type Storage MoveSfx = Map MoveSfx