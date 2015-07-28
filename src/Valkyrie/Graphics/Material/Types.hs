
module Valkyrie.Graphics.Material.Types where 

import Valkyrie.Math

import qualified Data.Map as Map
import Data.Typeable
import qualified Graphics.Rendering.OpenGL.Raw as GL

data TextureWrap = WrapClamp | WrapRepeat deriving Show

type TextureWrapS = TextureWrap
type TextureWrapT = TextureWrap

data Texture = Texture2D GL.GLuint deriving Show

data MaterialParam = MPF1 NReal | MPF2 Vector2 | MPF3 Vector3 | MPF4 Vector4 | MPTexture Texture deriving Show

data Material = Material { params :: Map.Map String MaterialParam } deriving (Show, Typeable)

data IntermediateParam = IPF1 (String, NReal) | IPF2 (String, Vector2) | IPF3 (String, Vector3) | IPF4 (String, Vector4) | IPTexture (String, String, TextureWrapS, TextureWrapT) deriving Show