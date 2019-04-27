{-# LANGUAGE TypeFamilyDependencies #-}

-- |
-- Module: Graphics.Primitives.Backend
-- Description: Type classes for graphics backends
-- Copyright: (c) Callum Lowcay 2019
-- License: BSD3
-- Maintainer: cwslowcay@gmail.com
--
-- This module contains typeclass declarations for graphics backends to
-- implement.
module Graphics.Primitives.Backend where

import           Control.Monad.Reader
import           Graphics.Primitives.Scene

-- | The rendering state of a backend.
type family RenderState b = r | r -> b

-- | The backend version of a Scene.
class Backend compiledScene where
  -- | Create state which is required during rendering and compilation operations.
  newRenderState :: IO (RenderState compiledScene)
  -- | Release rendering state.
  disposeRenderState :: RenderState compiledScene -> IO ()

  -- | Compile a scene to a backend specific data structure.
  compileScene :: Scene -> ReaderT (RenderState compiledScene) IO compiledScene
  -- | Recompile a scene.  This may be faster than compileScene if the backend performs caching.
  recompileScene :: compiledScene -> Scene -> ReaderT (RenderState compiledScene) IO compiledScene
  -- | Release resources associated with a compiled scene.
  disposeScene :: compiledScene -> IO ()

  -- | Render a compiled scene.
  renderScene :: compiledScene -> ReaderT (RenderState compiledScene) IO ()
