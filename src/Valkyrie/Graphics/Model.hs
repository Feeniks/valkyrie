
module Valkyrie.Graphics.Model(
    Valkyrie.Graphics.Model.Types.Model(..),
    Valkyrie.Graphics.Model.Types.modelMesh,
    Valkyrie.Graphics.Model.Types.modelMaterials,
    setInstance,
    removeInstance,
    getInstances
) where 

import Valkyrie.Types
import Valkyrie.Valkyrie
import Valkyrie.Resource
import Valkyrie.Resource.Types
import Valkyrie.Math
import Valkyrie.Binary
import Valkyrie.Graphics.Util
import Valkyrie.Graphics.Model.Types
import Valkyrie.Graphics.Mesh.Types
import Valkyrie.Graphics.Material.Types
import Valkyrie.Graphics.Mesh
import Valkyrie.Graphics.Material

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JT
import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Lens.TH
import Control.Monad
import Control.Monad.Trans
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Typeable
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL.Raw as GL

instance Resource Model where 
    load rm rs = do 
        (m,_) <- runLoad (loadModel rm) rs
        return m
        
    release m = return () --TODO: IMPL
    
instance Eq Model where 
    (==) ma mb = view (modelMesh.meshVBO) ma == view (modelMesh.meshVBO) mb
    
setInstance :: MonadIO m => Model -> String -> Matrix44 -> m ()
setInstance m n v = liftIO . atomically $ modifyTVar (m^.modelInstances) $ set (at n) (Just v)

removeInstance :: MonadIO m => Model -> String -> m ()
removeInstance m n = liftIO . atomically $ modifyTVar (m^.modelInstances) $ set (at n) Nothing

getInstances :: MonadIO m => Model -> m [Matrix44]
getInstances m = do 
    ins <- liftIO . atomically $ readTVar $ m ^. modelInstances
    return $ (fmap snd . M.toList) ins

loadModel :: (MonadIO m, ResourceStream rs) => ResourceManager -> Load rs m Model
loadModel rm = do 
    len <- remaining
    dta <- consume len
    let pres = parseOnly parseModel dta
    case pres of 
        Left e -> error $ show e
        Right r -> liftIO $ uncurry (createModel rm) $ r
        
createModel :: MonadIO m => ResourceManager -> String -> M.Map String String -> m Model --TODO: nice errors
createModel rm meshP matPs = do 
    mesh <- fmap fromJust $ obtainResourceInternal meshP rm
    mats <- sequence $ fmap (fmap fromJust . flip obtainResourceInternal rm) matPs
    ins <- liftIO . atomically $ newTVar M.empty
    return $ Model {
        _modelMesh = mesh,
        _modelMaterials = M.toList mats,
        _modelInstances = ins
    }
        
parseModel = do 
    meshP <- fmap B.unpack $ takeTill isEOL
    skipMany endOfLine
    mats <- many material
    return (meshP, M.fromList mats)

material = do 
    matN <- fmap B.unpack $ takeTill isSpace
    space
    matP <- fmap B.unpack $ takeTill isEOL
    skipMany endOfLine
    return (matN, matP)

isEOL c = c == '\r' || c == '\n'

readBS :: Read a => B.ByteString -> a
readBS = read . B.unpack
