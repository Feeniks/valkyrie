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
    init = return
    
    tick g = do 
        tme <- timer
        t <- timer
        liftIO . putStrLn $ show t
        ks <- keyState Key'Escape
        when (ks == KeyState'Pressed) exitValkyrie
        (Just mod) <- obtainResource "data/cube_test.mdl"
        setModel "t" mod $ (scale 2 2 2 <::> rotationY (Radians $ t ^. elapsedSeconds) <::> translate (-1.5) 0 0)
        setModel "u" mod $ (scale 2 2 2 <::> rotationY (Radians $ t ^. elapsedSeconds) <::> translate 1.5 0 0)
        return g

    shutdown _ = return ()
    
main :: IO ()
main = valkyrie TG
