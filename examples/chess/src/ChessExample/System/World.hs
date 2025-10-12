{-# LANGUAGE FieldSelectors  #-}

module ChessExample.System.World where

-- apecs-effectful
import Apecs.Effectful

import ChessExample.Component.Animation   (FadeOut, Forward, Path)
import ChessExample.Component.Camera      (Camera)
import ChessExample.Component.Environment (Environment)
import ChessExample.Component.Focus       (Focus)
import ChessExample.Component.Index       (Index)
import ChessExample.Component.Mesh        (Mesh)
import ChessExample.Component.Screen      (Screen)
import ChessExample.Component.Transform   (Transform)
import ChessExample.Component.Audio       

-- Type synonym that references all components. Used to delete entities, i.e. to
-- delete all components of an entity.
type All s =
  (
    ( Camera
    , Environment
    , FadeOut
    , Focus
    , Forward
    , Index
    , Mesh
    , Path
    ),
    ( Screen
    , Transform
    )
    , SoundRequest
    , PlayingChannel s
  )

data World s = World
  { camera         :: Unique Camera
  , environment    :: Unique Environment
  , fadeOut        :: Map FadeOut
  , focus          :: Map Focus
  , forward        :: Map Forward
  , index          :: Unique Index
  , mesh           :: Map Mesh
  , path           :: Map Path
  , screen         :: Unique Screen
  , transform      :: Map Transform
  , entityCounter  :: Storage EntityCounter
  -- Audio components
  , soundRequest   :: Map SoundRequest
  , playingChannel :: Map (PlayingChannel s)
  , sfxGroup       :: Storage  (SFXGroup s)
  , musicGroup     :: Storage  (MusicGroup s)
  }

instance Monad m => Has (World s) m Camera where
  getStore = SystemT (asks camera)

instance Monad m => Has (World s) m Environment where
  getStore = SystemT (asks environment)

instance Monad m => Has (World s) m FadeOut where
  getStore = SystemT (asks fadeOut)

instance Monad m => Has (World s) m Focus where
  getStore = SystemT (asks focus)

instance Monad m => Has (World s) m Forward where
  getStore = SystemT (asks forward) 

instance Monad m => Has (World s) m Index where
  getStore = SystemT (asks index)

instance Monad m => Has (World s) m Mesh where
  getStore = SystemT (asks mesh)

instance Monad m => Has (World s) m Path where
  getStore = SystemT (asks path)

instance Monad m => Has (World s) m Screen where
  getStore = SystemT (asks screen)

instance Monad m => Has (World s) m Transform where
  getStore = SystemT (asks transform)

instance Monad m => Has (World s) m SoundRequest where
  getStore :: Monad m => SystemT (World s) m (Storage SoundRequest)
  getStore = SystemT (asks soundRequest)

instance Monad m => Has (World s) m (PlayingChannel s) where
  getStore = SystemT (asks playingChannel)

instance Monad m => Has (World s) m (SFXGroup s) where
  getStore = SystemT (asks sfxGroup)

instance (Monad m) => Has (World s) m (MusicGroup s)  where
  getStore = SystemT (asks musicGroup)

instance (Monad m) => Has (World s) m EntityCounter where
   getStore = SystemT (asks entityCounter)
  
initWorld :: IO (World s)
initWorld =
  World
  <$> explInit -- Unique Camera
  <*> explInit -- Unique Environment
  <*> explInit -- Map FadeOut
  <*> explInit -- Map Focus
  <*> explInit -- Map Forward
  <*> explInit -- Unique Index
  <*> explInit -- Map Mesh
  <*> explInit -- Map Path
  <*> explInit -- Unique Screen
  <*> explInit -- Map Transform
  <*> explInit -- ReadOnly (Global EntityCounter)
  <*> explInit -- Map SoundRequest
  <*> explInit -- Map (PlayingChannel s)
  <*> explInit -- Global (SFXGroup s)
  <*> explInit -- Global (MusicGroup s)