{-# LANGUAGE TypeFamilies #-}
module ChessExample.Component.Audio where

import Apecs.Effectful (Component(..), Global, Map)

-- | Fire-and-forget sound effects (consumed in the same tick).
data MoveSfx    = MoveSfx

instance Component MoveSfx
    where type Storage MoveSfx = Map MoveSfx