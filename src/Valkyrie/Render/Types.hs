{-# LANGUAGE TemplateHaskell #-}

module Valkyrie.Render.Types where 

import Valkyrie.Math
import Valkyrie.Graphics.Model.Types

import Control.Lens.TH
import qualified Data.Map as M

data RenderWorld = RenderWorld {
    _rwView :: Matrix44,
    _rwProj :: Matrix44,
    _rwModels :: M.Map String (Model,Matrix44)
}

makeLenses ''RenderWorld
