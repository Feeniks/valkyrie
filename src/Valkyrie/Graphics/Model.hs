
module Valkyrie.Graphics.Model(
    Model
) where 

import Valkyrie.Types
import Valkyrie.Valkyrie
import Valkyrie.Resource
import Valkyrie.Resource.Types
import Valkyrie.Graphics.Util
import Valkyrie.Math
import Valkyrie.Binary
import Valkyrie.Graphics.Mesh
import Valkyrie.Graphics.Material

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JT
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Data.Map as M
import Data.Typeable
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL.Raw as GL

data Model = Model {
    _modelMesh :: Mesh,
    _modelMaterials :: M.Map String Material
}

makeLenses ''Model

instance Resource Model where 
    load rm rs = undefined
    release m = undefined
