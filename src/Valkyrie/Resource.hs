{-# LANGUAGE ExistentialQuantification #-}

module Valkyrie.Resource(
    createResourceManager,
    attachResourceLocator, 
    attachLocator,
    obtainResource,
    releaseResource
) where 

import Valkyrie.Types
import Valkyrie.Resource.Types
import Valkyrie.Valkyrie

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Typeable
import Debug.Trace

instance (Monad m) => Functor (Load s m) where 
    fmap = liftM

instance (Monad m) => Applicative (Load s m) where
    pure = return
    (<*>) = ap

instance (Monad m) => Monad (Load s m) where 
    return x = Load $ \s -> return (x,s)
    (>>=) x f = Load $ \s -> (runLoad x) s >>= \(v,s') -> runLoad (f v) s'

instance (MonadIO m) => MonadIO (Load s m) where 
    liftIO x = Load $ \s -> liftIO x >>= \v -> return (v,s)

createResourceManager :: IO ResourceManager
createResourceManager = atomically $ fmap (ResourceManager []) $ newTVar M.empty

attachResourceLocator :: ResourceLocator l => l -> ValkyrieM IO ()
attachResourceLocator l = modify $ \valk -> do 
    return $ valk & valkResourceManager %~ (attachLocator l)
    
attachLocator :: ResourceLocator l => l -> ResourceManager -> ResourceManager
attachLocator l (ResourceManager lx rx) = ResourceManager (RLB l:lx) rx

obtainResource :: (Resource r, Typeable r) => String -> ValkyrieM IO (Maybe r)
obtainResource path = do 
    rm@(ResourceManager lx rx) <- fmap (view valkResourceManager) get
    rmap <- liftIO . atomically $ readTVar rx
    maybe (obtainNew path rm) (\(RB r) -> return $ cast r) $ M.lookup path rmap
    
releaseResource :: String -> ValkyrieM IO ()
releaseResource path = do 
    (ResourceManager _ rx) <- fmap (view valkResourceManager) get
    rmap <- liftIO . atomically $ readTVar rx
    maybe (return ()) (\(RB r) -> release r) $ M.lookup path rmap
    liftIO . atomically $ modifyTVar rx $ M.delete path
    
obtainNew :: (Resource r, Typeable r) => String -> ResourceManager -> ValkyrieM IO (Maybe r)
obtainNew path rm@(ResourceManager [] rx) = return Nothing
obtainNew path rm@(ResourceManager ((RLB l):lx) rx) = do
    mres <- resolve rm path l
    case mres of 
        Nothing -> obtainNew path $ ResourceManager lx rx
        Just r -> do 
            liftIO . atomically . modifyTVar rx $ insert path r
            return mres
            
insert :: (Resource r, Typeable r) => String -> r -> ResourceMap -> ResourceMap
insert k v = M.insert k $ RB v
