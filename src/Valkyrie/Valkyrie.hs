
module Valkyrie.Valkyrie where 

import Valkyrie.Types

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

instance Monad m => Functor (ValkyrieM m) where 
    fmap = liftM

instance Monad m => Applicative (ValkyrieM m) where 
    pure = return
    (<*>) = ap

instance Monad m => Monad (ValkyrieM m) where 
    return x = ValkyrieM $ \v -> return (x, v)
    (>>=) (ValkyrieM x) f = ValkyrieM $ \v -> x v >>= \(x', v') -> (runValkyrieM (f x')) v'

instance MonadTrans ValkyrieM where 
    lift mx = ValkyrieM $ \v -> mx >>= \x -> return (x, v)

instance (MonadIO m) => MonadIO (ValkyrieM m) where 
    liftIO mx = ValkyrieM $ \v -> liftIO mx >>= \x -> return (x, v)

get :: Monad m => ValkyrieM m Valkyrie
get = ValkyrieM $ \v -> return (v, v)

put :: Monad m => Valkyrie -> ValkyrieM m ()
put v = ValkyrieM $ \_ -> return ((), v)

modify :: Monad m => (Valkyrie -> m Valkyrie) -> ValkyrieM m ()
modify f = do 
    valk <- get
    valk' <- lift $ f valk
    put valk'