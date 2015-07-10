{-# LANGUAGE TemplateHaskell #-}

module Valkyrie.Timer.Types where 

import Control.Lens.TH

data Timer = Timer {
    _elapsedSeconds :: Double,
    _elapsedFrames :: Int,
    _frameDuration :: Double
} deriving Show

makeLenses ''Timer
