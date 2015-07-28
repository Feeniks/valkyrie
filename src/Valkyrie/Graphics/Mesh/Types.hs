{-# LANGUAGE TemplateHaskell #-}

module Valkyrie.Graphics.Mesh.Types where 

import Control.Lens.TH
import qualified Data.Map as M
import Data.Typeable
import qualified Graphics.Rendering.OpenGL.Raw as GL

data Mesh = Mesh {
    _meshVertexCount :: GL.GLsizei,
    _meshVBO :: GL.GLuint,
    _meshParts :: M.Map String (Int, GL.GLuint)
} deriving (Show, Typeable)

makeLenses ''Mesh
