
module Valkyrie.Graphics.Shader(
    Shader(..),
    VertexShader,
    PixelShader
) where 

import Valkyrie.Types
import Valkyrie.Valkyrie
import Valkyrie.Resource
import Valkyrie.Resource.Types
import Valkyrie.Graphics.Util

import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL.Raw as GL

class Shader s where 
    shaderID :: s -> GL.GLuint

newtype VertexShader = VS GL.GLuint deriving Show
newtype PixelShader = PS GL.GLuint deriving Show

instance Shader VertexShader where 
    shaderID (VS i) = i
    
instance Shader PixelShader where 
    shaderID (PS i) = i

instance Resource VertexShader where 
    load _ rs = do 
        (s,_) <- runLoad (loadShader GL.gl_VERTEX_SHADER) rs
        return $ VS s
    
    release s = return () --TODO: IMPL
    
instance Resource PixelShader where 
    load _ rs = do 
        (s,_) <- runLoad (loadShader GL.gl_FRAGMENT_SHADER) rs
        return $ PS s
    
    release s = return () --TODO: IMPL
    
bindVertexShader :: VertexShader -> ValkyrieM IO ()
bindVertexShader (VS s) = undefined --TODO: IMPL

bindPixelShader :: PixelShader -> ValkyrieM IO ()
bindPixelShader (PS s) = undefined --TODO: IMPL
    
loadShader :: (MonadIO m, ResourceStream rs) => GL.GLenum -> Load rs m GL.GLuint
loadShader typ = do 
    len <- remaining
    src <- fmap B.unpack $ consume len
    sid <- liftIO $ GL.glCreateShader typ
    liftIO (withCString src $ \ptr1 -> with ptr1 $ \ptr2 -> GL.glShaderSource sid 1 ptr2 nullPtr)
    liftIO $ GL.glCompileShader sid
    merr <- liftIO $ compileError sid
    case merr of 
        Right _ -> return sid
        Left e -> error e
    
compileError id = do 
    ok <- liftM toBool $ fetch GL.gl_COMPILE_STATUS
    case ok of 
        True -> return $ Right ()
        False -> do 
            ll <- fetch GL.gl_INFO_LOG_LENGTH
            allocaArray0 (fromIntegral ll) $ \mptr -> GL.glGetShaderInfoLog id ll nullPtr mptr >> peekCString mptr >>= return . Left
    where 
    fetch i = onPtr $ GL.glGetShaderiv id i
