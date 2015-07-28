{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Valkyrie
import Valkyrie.Resource.Types

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import Data.Typeable
import Prelude hiding (init)

data TestGame = TG

instance Game TestGame where 
    init g = do 
        (Just mod) <- obtainResource "data/cube_test.mdl"
        useModel mod
        return g
    
    tick g = do 
        tme <- timer
        t <- timer
        liftIO . putStrLn $ show t
        ks <- keyState Key'Escape
        when (ks == KeyState'Pressed) exitValkyrie
        (Just mod) <- obtainResource "data/cube_test.mdl"
        setInstance mod "t" $ (scale 2 2 2 <::> rotationY (Radians $ t ^. elapsedSeconds) <::> translate (-1.5) 0 0)
        setInstance mod "u" $ (scale 2 2 2 <::> rotationY (Radians $ t ^. elapsedSeconds) <::> translate 1.5 0 0)
        return g

    shutdown _ = return ()
        
main :: IO ()
main = valkyrie TG
