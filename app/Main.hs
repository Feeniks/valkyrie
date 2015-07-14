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
        res1 <- obtainResource "data/valkyrie.vs" :: ValkyrieM IO (Maybe VertexShader)
        res2 <- obtainResource "data/valkyrie.ps" :: ValkyrieM IO (Maybe PixelShader)
        liftIO . putStrLn $ show res1
        liftIO . putStrLn $ show res2
        ks <- keyState Key'Escape
        when (ks == KeyState'Pressed) exitValkyrie
        return g

    shutdown _ = do 
        releaseResource "data/valkyrie.vs"
        releaseResource "data/valkyrie.ps"

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
    release b = return ()
        
main :: IO ()
main = valkyrie TG
