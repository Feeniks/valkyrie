{-# LANGUAGE TemplateHaskell #-}

module Valkyrie.Graphics.Mesh(
    Mesh,
    bindMesh,
    drawMeshPart
) where 

import Valkyrie.Types
import Valkyrie.Valkyrie
import Valkyrie.Resource
import Valkyrie.Resource.Types
import Valkyrie.Graphics.Util

import Control.Lens
import Control.Lens.TH
import Control.Monad
import Control.Monad.Trans
import qualified Data.Map as M
import Data.Traversable (sequence)
import Data.Typeable
import Foreign
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL.Raw as GL
import Text.Parsec

-- TODO: use attoparsec

data Mesh = Mesh {
    _meshVertexCount :: GL.GLsizei,
    _meshVBO :: GL.GLuint,
    _meshParts :: M.Map String (Int, GL.GLuint)
} deriving (Show, Typeable)

makeLenses ''Mesh

instance Resource Mesh where 
    load rm rs = do 
        (m,_) <- runLoad (loadMesh rm) rs
        return m
        
    release m = return () --TODO: IMPL
    
csize :: Int
csize = sizeOf (undefined :: GL.GLfloat)
    
bindMesh :: MonadIO m => Mesh -> m ()
bindMesh m = liftIO $ do 
    GL.glBindBuffer GL.gl_ARRAY_BUFFER $ m ^. meshVBO
    let stride = 8 * csize
    enableVertexAttrib 0 0 stride 3
    enableVertexAttrib 1 (3 * csize) stride 3
    enableVertexAttrib 2 (6 * csize) stride 2

drawMeshPart :: MonadIO m => String -> Mesh -> m ()
drawMeshPart n m = liftIO $ do 
    let ibo = m ^. (meshParts.at n)
    maybe (return ()) draw ibo
    where 
    draw (c, i) = do 
        GL.glBindBuffer GL.gl_ELEMENT_ARRAY_BUFFER i
        GL.glDrawElements GL.gl_TRIANGLES (fromIntegral c) GL.gl_UNSIGNED_INT nullPtr

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

createIBO :: [Int32] -> IO (Int, GL.GLuint) --TODO: check for errors
createIBO dx = do 
    bid <- onPtr (GL.glGenBuffers 1)
    GL.glBindBuffer GL.gl_ELEMENT_ARRAY_BUFFER bid
    withArrayLen dx $ dfill
    return (length dx, bid) 
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

enableVertexAttrib :: Int -> Int -> Int -> Int -> IO ()
enableVertexAttrib ix offset stride nc = do 
    let index = fromIntegral ix
    GL.glEnableVertexAttribArray index
    GL.glVertexAttribPointer index (fromIntegral nc) GL.gl_FLOAT (fromBool False) (fromIntegral stride) $ plusPtr nullPtr offset

