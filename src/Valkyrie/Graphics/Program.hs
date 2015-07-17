{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}

module Valkyrie.Graphics.Program( 
    Program,
    createProgram,
    useProgram,
    bindFloat1,
    bindFloat2,
    bindFloat3,
    bindFloat4,
    bindMatrix44,
    bindTexture
) where 

import Valkyrie.Types
import Valkyrie.Valkyrie
import Valkyrie.Resource
import Valkyrie.Resource.Types
import Valkyrie.Graphics.Util
import Valkyrie.Graphics.Shader
import Valkyrie.Graphics.Material
import Valkyrie.Math

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.Typeable
import qualified Data.Vector.Storable as V
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL.Raw as GL

newtype Program = Program GL.GLuint deriving (Show, Typeable)

data ProgramSource = PVS String | PPS String

data ShaderB = forall s. Shader s => SB s

instance Resource Program where 
    load rm rs = do
        (p,_) <- runLoad (ld rm) rs
        return p
        
    release p = return () --TODO: IMPL

createProgram :: MonadIO m => [ShaderB] -> m Program
createProgram sx = liftIO $ do
    let sids = map (\(SB s) -> shaderID s) sx
    pid <- GL.glCreateProgram
    mapM_ (GL.glAttachShader pid) sids
    GL.glLinkProgram pid
    ok <- status pid
    case ok of 
        Left e -> error e
        Right _ -> return $ Program pid
        
useProgram :: MonadIO m => Program -> m ()
useProgram (Program pid) = GL.glUseProgram pid
        
bindFloat1 :: MonadIO m => Program -> String -> NReal -> m ()
bindFloat1 p k v = do 
    loc <- paramLoc p k
    GL.glUniform1f loc $ realToFrac v
    
bindFloat2 :: MonadIO m => Program -> String -> Vector2 -> m ()
bindFloat2 p k (V2 x y) = do 
    loc <- paramLoc p k
    GL.glUniform2f loc (realToFrac x) (realToFrac y)
    
bindFloat3 :: MonadIO m => Program -> String -> Vector3 -> m ()
bindFloat3 p k (V3 x y z) = do 
    loc <- paramLoc p k
    GL.glUniform3f loc (realToFrac x) (realToFrac y) (realToFrac z)
    
bindFloat4 :: MonadIO m => Program -> String -> Vector4 -> m ()
bindFloat4 p k (V4 x y z w) = do 
    loc <- paramLoc p k
    GL.glUniform4f loc (realToFrac x) (realToFrac y) (realToFrac z) (realToFrac w)
    
bindMatrix44 :: MonadIO m => Program -> String -> Matrix44 -> m ()
bindMatrix44 p k m = do 
    loc <- paramLoc p k
    let mv = V.fromList $ toGL m
    liftIO $ V.unsafeWith mv $ \ptr -> GL.glUniformMatrix4fv loc 1 0 ptr

bindTexture :: MonadIO m => Program -> String -> Texture -> m ()
bindTexture p k (Texture2D tid) = do 
    loc <- paramLoc p k
    GL.glActiveTexture GL.gl_TEXTURE0
    GL.glBindTexture GL.gl_TEXTURE_2D tid
    GL.glUniform1i loc 0

paramLoc :: MonadIO m => Program -> String -> m GL.GLint
paramLoc (Program pid) k = liftIO $ withGLstring k $ GL.glGetUniformLocation pid
        
status :: GL.GLuint -> IO (Either String ())
status id = do 
    ok <- liftM toBool $ onPtr $ GL.glGetProgramiv id GL.gl_LINK_STATUS
    case ok of 
        True -> return $ Right ()
        False -> do 
            ll <- onPtr $ GL.glGetProgramiv id GL.gl_INFO_LOG_LENGTH
            allocaArray0 (fromIntegral ll) $ \mptr -> GL.glGetProgramInfoLog id ll nullPtr mptr >> peekCString mptr >>= return . Left

ld :: (MonadIO m, ResourceStream rs) => ResourceManager -> Load rs m Program
ld rm = do 
    len <- remaining
    buf <- consume len
    case (parseOnly (many psource) buf) of 
        Left pe -> error $ show pe
        Right sx -> do 
            shaders <- mapM (loadShader rm) sx
            createProgram shaders
            
loadShader :: MonadIO m => ResourceManager -> ProgramSource -> m ShaderB
loadShader rm (PVS p) = do 
    ms <- obtainResourceInternal p rm
    maybe (error $ "Could not load shader: " ++ p) (return . boxVS) ms
loadShader rm (PPS p) = do 
    ms <- obtainResourceInternal p rm
    maybe (error $ "Could not load shader: " ++ p) (return . boxPS) ms
    
--Uuuurgh
boxVS :: VertexShader -> ShaderB
boxVS = SB

boxPS :: PixelShader -> ShaderB
boxPS = SB

psource = pvs <|> pps

pvs = do 
    string "VS "
    path <- takeTill isEOL
    skipWhile isEOL
    return . PVS $ B.unpack path
    
pps = do 
    string "PS "
    path <- takeTill isEOL
    skipWhile isEOL
    return . PPS $ B.unpack path

isEOL = (\c -> c == '\r' || c == '\n')