{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module ChessExample.Game where

-- apecs-effectful
import Apecs.Effectful (runECS)

-- effectful-core
import Effectful                    (Eff, IOE)
import Effectful.State.Static.Local (evalState)

-- hagato:with-core.effectful
import Hagato.Core.Effectful (Log, loopUntil_)

-- hagato:with-vulkan
import Hagato.Vulkan.PhysicalDevice (PhysicalDevice)
import Hagato.Vulkan.Queue          (Queue)
import Hagato.Vulkan.Swapchain      (Swapchain)

-- hagato:with-vulkan-effectful
import Hagato.Vulkan.Effectful (Memory, Window, manageWindow, tickPoll)

-- resource-effectful
import Effectful.Resource (Resource)

import ChessExample.GameState       (GameState(..), newGameState)
import ChessExample.Component.Audio 
import ChessExample.System.Artist   (render)
import ChessExample.System.Asset    (loadScene)
import ChessExample.System.Input    (process)
import ChessExample.System.World    (initWorld)
import ChessExample.System.Mixer    
import ChessExample.Vulkan.Memory   (manageMemoryAllocator)
import ChessExample.Vulkan.Renderer (allocateRenderer)
import ChessExample.Vulkan.Setup    (manageRenderSetup, withRenderSetup)
import ChessExample.Sounds
import Effectful.Extra              (type (<:))
import UnifiedAudio.Effectful
import Data.Typeable (Typeable)

-- This is where the actual code for the game begins. The game implementation is
-- independent of concrete strategies for logging, GPU memory allocation, windowing
-- and debugging. These strategies will be set near the main function, which allows
-- us to switch the implementations easily (e.g., logging to a file instead of stdout),
-- maybe even via separate build configurations where only the main function needs to
-- be rebuilt.
game
  :: forall es s a k w. ( Typeable s,
       es <: IOE
     , es <: Log PhysicalDevice
     , es <: Log Swapchain
     , es <: Log Queue
     , es <: Memory a k
     , es <: Resource
     , es <: Window w
     , es <: Audio s
     )
  => [String] -> Eff es ()
game layers =
  evalState 0 . runECS (initWorld @s) $ do
    window      <- manageWindow 800 600 "Left Click: Play | Right Click: Rotate | Scroll: Zoom | Backspace: Take Back"
    renderSetup <- manageRenderSetup window layers
    allocator   <- manageMemoryAllocator renderSetup
    renderer    <- allocateRenderer renderSetup allocator 3
    meshFactory <- loadScene renderer allocator 800 600
    sounds      <- initSounds
    initState   <- newGameState renderer meshFactory
    playMusic Music
    withRenderSetup renderSetup $
      loopUntil_ (.done) initState $
        \dt state -> do
          input     <- tickPoll dt window
          state'    <- process meshFactory input state
          audioSystem sounds
          renderer' <- render allocator state'.renderer
          pure $ state' { renderer = renderer' }