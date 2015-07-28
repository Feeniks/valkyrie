{-# LANGUAGE TemplateHaskell #-}

module Valkyrie.Render.Types where 

import Valkyrie.Math
import Valkyrie.Graphics.Model.Types

import Control.Lens.TH

data RenderWorld = RenderWorld {
    _rwView :: Matrix44,
    _rwProj :: Matrix44,
    _rwModels :: [Model]
}

makeLenses ''RenderWorld
