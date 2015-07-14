{-# LANGUAGE ExistentialQuantification #-}

module Valkyrie.Resource.Types where 

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Typeable

newtype Load s m a = Load { runLoad :: s -> m (a, s) }

class ResourceStream rs where 
    remaining :: MonadIO m => Load rs m Int
    consume :: MonadIO m => Int -> Load rs m B.ByteString

class Resource r where 
    load :: (MonadIO m, ResourceStream rs) => ResourceManager -> rs -> m r
    release :: MonadIO m => r -> m ()

class ResourceLocator rl where 
    resolve :: (MonadIO m, Resource r) => ResourceManager -> String -> rl -> m (Maybe r)

data ResourceB = forall r. (Resource r, Typeable r) => RB r

type ResourceMap = M.Map String ResourceB

data ResourceLocatorB = forall l. ResourceLocator l => RLB l

data ResourceManager = ResourceManager [ResourceLocatorB] (TVar ResourceMap)
