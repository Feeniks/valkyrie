
module Valkyrie.Graphics.Mesh(
    Valkyrie.Graphics.Mesh.Types.Mesh,
    bindMeshVBO,
    bindMeshPart,
    drawMeshPart
) where 

import Valkyrie.Types
import Valkyrie.Valkyrie
import Valkyrie.Resource
import Valkyrie.Resource.Types
import Valkyrie.Graphics.Util
import Valkyrie.Graphics.Mesh.Types

import Control.Applicative
import Control.Lens
import Control.Lens.TH
import Control.Monad
import Control.Monad.Trans
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Traversable (sequence)
import Foreign
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL.Raw as GL

instance Resource Mesh where 
    load rm rs = do 
        (m,_) <- runLoad (loadMesh rm) rs
        return m
        
    release m = return () --TODO: IMPL
    
bindMeshVBO :: MonadIO m => Mesh -> m ()
bindMeshVBO m = liftIO $ GL.glBindBuffer GL.gl_ARRAY_BUFFER $ m ^. meshVBO

bindMeshPart :: MonadIO m => String -> Mesh -> m ()
bindMeshPart n m = liftIO $ do 
    let ibo = m ^. (meshParts.at n)
    maybe (return ()) bind ibo
    where 
    bind (c, i) = GL.glBindBuffer GL.gl_ELEMENT_ARRAY_BUFFER i

drawMeshPart :: MonadIO m => String -> Mesh -> m ()
drawMeshPart n m = liftIO $ do 
    let ibo = m ^. (meshParts.at n)
    maybe (return ()) draw ibo
    where 
    draw (c, i) = GL.glDrawElements GL.gl_TRIANGLES (fromIntegral c) GL.gl_UNSIGNED_INT nullPtr

loadMesh :: (MonadIO m, ResourceStream rs) => ResourceManager -> Load rs m Mesh
loadMesh rm = do 
    len <- remaining
    dta <- consume len
    let pres = parseOnly parseMesh dta
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
    vcount <- fmap readBS $ takeTill isEOL
    endOfLine
    verts <- replicateM (vcount * 8) component :: Parser [Double]
    nparts <- fmap readBS $ takeTill isEOL
    endOfLine
    parts <- fmap M.fromList $ replicateM nparts part
    return (fmap realToFrac verts, parts)
    
part = do 
    pname <- fmap B.unpack $ takeTill isEOL
    endOfLine
    ninds <- fmap readBS $ takeTill isEOL
    endOfLine
    ix <- replicateM ninds component :: Parser [Int]
    let inds = fmap fromIntegral ix
    return (pname, inds)

component :: Read a => Parser a
component = do 
    c <- takeTill (\c -> isEOL c || c == ' ')
    whitespace
    return $ readBS c
    
whitespace = do 
    skipMany space
    skipMany endOfLine

isEOL c = c == '\r' || c == '\n'

readBS :: Read a => B.ByteString -> a
readBS = read . B.unpack
