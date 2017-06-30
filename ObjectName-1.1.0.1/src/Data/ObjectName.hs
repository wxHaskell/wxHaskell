{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.ObjectName
-- Copyright   :  (c) Sven Panne 2015
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Object names are explicitly handled identifiers for API objects, e.g. a
-- texture object name in OpenGL or a buffer object name in OpenAL. They come in
-- two flavors: If a name can exist on its own without an associated object, we
-- have a 'GeneratableObjectName', otherwise we have an 'ObjectName'.
--
--------------------------------------------------------------------------------

module Data.ObjectName (
   ObjectName(..), GeneratableObjectName(..)
) where

import Control.Monad ( replicateM )
import Control.Monad.IO.Class ( MonadIO(..) )

--------------------------------------------------------------------------------

-- | An 'ObjectName' is an explicitly handled identifier for API objects, e.g. a
-- texture object name in OpenGL or a buffer object name in OpenAL.
#if __GLASGOW_HASKELL__ < 708
--
-- Minimal complete definition: 'isObjectName' plus one of 'deleteObjectName' or
-- 'deleteObjectNames'.
#endif

class ObjectName a where
#if __GLASGOW_HASKELL__ >= 708
   {-# MINIMAL isObjectName, ( deleteObjectName | deleteObjectNames ) #-}
#endif
   -- | Test if the given object name is currently in use, i.e. test if it has
   -- been generated, but not been deleted so far.
   isObjectName :: MonadIO m => a -> m Bool

   -- | Make the given object name available again, declaring it as unused.
   deleteObjectName :: MonadIO m => a -> m ()
   deleteObjectName = deleteObjectNames . (:[])

   -- | Bulk version of 'deleteObjectName'.
   deleteObjectNames:: MonadIO m => [a] -> m ()
   deleteObjectNames = mapM_ deleteObjectName

-- | A 'GeneratableObjectName' is an 'ObjectName' which can be generated without
-- creating an associated object at the same time, e.g. an OpenGL buffer object
-- name. Note that e.g. OpenGL program object names do not fall into this
-- category, because you can only create such a name together with a program
-- object itself.
#if __GLASGOW_HASKELL__ < 708
--
-- Minimal complete definition: One of 'genObjectName' or 'genObjectNames'.
#endif

class ObjectName a => GeneratableObjectName  a where
#if __GLASGOW_HASKELL__ >= 708
   {-# MINIMAL genObjectName | genObjectNames #-}
#endif
   -- | Generate a new unused object name. By generating the name, it becomes
   -- used.
   genObjectName :: MonadIO m => m a
   genObjectName = liftIO . fmap head . genObjectNames $ 1

   -- | Bulk version of 'genObjectName'.
   genObjectNames :: MonadIO m => Int -> m [a]
   genObjectNames = flip replicateM genObjectName
