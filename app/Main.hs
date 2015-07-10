{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Valkyrie
import Valkyrie.Resource
import Valkyrie.Resource.Types

import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import Data.Typeable
import Prelude hiding (init)

data TestGame = TG

instance Game TestGame where 
    init = return 
    
    tick g = do 
        tme <- timer
        res <- obtainResource "stack.yaml" :: ValkyrieM IO (Maybe Binary)
        liftIO . putStrLn $ show res
        ks <- keyState Key'Escape
        when (ks == KeyState'Pressed) exitValkyrie
        return g

    shutdown _ = return ()

data Binary = Binary B.ByteString deriving (Show, Typeable)

ld :: (MonadIO m, ResourceStream rs) => Load rs m Binary
ld = do 
    len <- remaining
    buf <- consume len
    return $ Binary buf

instance Resource Binary where 
    load rm rs = do
        (b,_) <- runLoad ld rs
        return b
        
main :: IO ()
main = valkyrie TG
