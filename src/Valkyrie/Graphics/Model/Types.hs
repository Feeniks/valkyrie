{-# LANGUAGE TemplateHaskell #-}

module Valkyrie.Graphics.Model.Types where 

import Valkyrie.Math
import Valkyrie.Graphics.Mesh.Types
import Valkyrie.Graphics.Material.Types

import Control.Lens.TH
import qualified Data.Map as M

data Model = Model {
    _modelMesh :: Mesh,
    _modelMaterials :: [(String, Material)]
}

makeLenses ''Model
