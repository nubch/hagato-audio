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
import UnifiedAudio.Effectful (mute)

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
    , MasterGain
    , SetMasterGain
    , BaseVolume
  )

data World s = World
  { camera         :: Storage Camera
  , environment    :: Storage Environment
  , fadeOut        :: Storage FadeOut
  , focus          :: Storage Focus
  , forward        :: Storage Forward
  , index          :: Storage Index
  , mesh           :: Storage Mesh
  , path           :: Storage Path
  , screen         :: Storage Screen
  , transform      :: Storage Transform
  , entityCounter  :: Storage EntityCounter
  -- Audio components
  , soundRequest   :: Storage SoundRequest
  , playingChannel :: Storage (PlayingChannel s)
  , masterGain     :: Storage MasterGain
  , setMasterGain  :: Storage SetMasterGain
  , baseVolume     :: Storage BaseVolume
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
  getStore = SystemT (asks soundRequest)

instance Monad m => Has (World s) m (PlayingChannel s) where
  getStore = SystemT (asks playingChannel)

instance Monad m => Has (World s) m EntityCounter where
   getStore = SystemT (asks entityCounter)

instance Monad m => Has (World s) m MasterGain where
   getStore = SystemT (asks masterGain)

instance Monad m => Has (World s) m SetMasterGain where
   getStore = SystemT (asks setMasterGain)
  
instance Monad m => Has (World s) m BaseVolume where
   getStore = SystemT (asks baseVolume)

initWorld :: IO (World s)
initWorld = 
  World 
  <$> explInit
  <*> explInit
  <*> explInit
  <*> explInit
  <*> explInit
  <*> explInit
  <*> explInit
  <*> explInit
  <*> explInit
  <*> explInit
  <*> explInit
  <*> explInit  
  <*> explInit  
  <*> explInit  
  <*> explInit  
  <*> explInit

--makeWorld "World"
  --[ ''Camera
  --, ''Environment
  --, ''FadeOut
  --, ''Focus
  --, ''Forward
  --, ''Index
  --, ''Mesh
  --, ''Path
  --, ''Screen
  --, ''Transform,
    ---- Audio components
    --''SoundRequest
  --]
