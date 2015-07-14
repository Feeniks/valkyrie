{-# LANGUAGE TemplateHaskell #-}

module Valkyrie.Graphics.Mesh(
    Mesh,
    bindMesh,
    bindMeshPart
) where 

import Valkyrie.Types
import Valkyrie.Valkyrie
import Valkyrie.Resource
import Valkyrie.Resource.Types
import Valkyrie.Graphics.Util

import Control.Lens.TH
import Control.Monad
import Control.Monad.Trans
import qualified Data.Map as M
import Data.Traversable (sequence)
import Foreign
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL.Raw as GL
import Text.Parsec

data Mesh = Mesh {
    _meshVertexCount :: GL.GLsizei,
    _meshVBO :: GL.GLuint,
    _meshParts :: M.Map String GL.GLuint
} deriving Show

makeLenses ''Mesh

instance Resource Mesh where 
    load rm rs = do 
        (m,_) <- runLoad (loadMesh rm) rs
        return m
        
    release m = return () --TODO: IMPL
    
bindMesh :: Mesh -> ValkyrieM IO ()
bindMesh m = undefined

bindMeshPart :: String -> Mesh -> ValkyrieM IO ()
bindMeshPart n m = undefined

loadMesh :: (MonadIO m, ResourceStream rs) => ResourceManager -> Load rs m Mesh
loadMesh rm = do 
    len <- remaining
    dta <- consume len
    let pres = parse parseMesh "" dta
    case pres of 
        Left e -> error $ show e
        Right r -> liftIO $ uncurry createMesh $ r
        
createMesh :: [GL.GLfloat] -> M.Map String [Int32] -> IO Mesh
createMesh verts parts = do 
    vbo <- createVBO verts
    mparts <- sequence $ fmap createIBO parts
    return $ Mesh {
        _meshVertexCount = div (fromIntegral $ length verts) 8,
        _meshVBO = vbo,
        _meshParts = mparts
    }

createVBO :: [GL.GLfloat] -> IO GL.GLuint --TODO: check for errors
createVBO dx = do 
    bid <- onPtr (GL.glGenBuffers 1)
    GL.glBindBuffer GL.gl_ARRAY_BUFFER bid
    withArrayLen dx $ dfill
    return bid 
    where 
    dfill l ptr = GL.glBufferData GL.gl_ARRAY_BUFFER (fromIntegral (l * sizeOf (undefined :: GL.GLfloat))) (ptr :: Ptr GL.GLfloat) GL.gl_STATIC_DRAW

createIBO :: [Int32] -> IO GL.GLuint --TODO: check for errors
createIBO dx = do 
    bid <- onPtr (GL.glGenBuffers 1)
    GL.glBindBuffer GL.gl_ELEMENT_ARRAY_BUFFER bid
    withArrayLen dx $ dfill
    return bid 
    where 
    dfill l ptr = GL.glBufferData GL.gl_ELEMENT_ARRAY_BUFFER (fromIntegral (l * sizeOf (undefined :: Int32))) (ptr :: Ptr Int32) GL.gl_STATIC_DRAW
    
parseMesh = do 
    vcount <- fmap read $ manyTill digit crlf
    verts <- fmap concat $ count vcount vertex
    nparts <- fmap read $ manyTill digit crlf
    parts <- fmap M.fromList $ count nparts part
    return (verts, parts)
    
vertex = do 
    raw <- manyTill anyChar crlf
    let dverts = (fmap read (words raw) :: [Double])
    let verts = fmap realToFrac dverts
    return verts
    
part = do 
    pname <- manyTill anyChar crlf
    raw <- manyTill anyChar crlf
    let inds = fmap (fromIntegral . read) (words raw)
    return (pname, inds)
