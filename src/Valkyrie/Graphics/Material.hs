{-# LANGUAGE DeriveDataTypeable #-}

module Valkyrie.Graphics.Material (
    Texture(..), 
    MaterialParam(..), 
    Material(..)
) where 

import Valkyrie.Types
import Valkyrie.Valkyrie
import Valkyrie.Resource
import Valkyrie.Resource.Types
import Valkyrie.Graphics.Util
import Valkyrie.Math
import Valkyrie.Binary

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JT
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import Data.Typeable
import qualified Data.Vector.Storable as V
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL.Raw as GL

data TextureWrap = WrapClamp | WrapRepeat deriving Show

type TextureWrapS = TextureWrap
type TextureWrapT = TextureWrap

data Texture = Texture2D GL.GLuint deriving Show

data MaterialParam = MPF1 NReal | MPF2 Vector2 | MPF3 Vector3 | MPF4 Vector4 | MPTexture Texture deriving Show

data Material = Material { params :: Map.Map String MaterialParam } deriving (Show, Typeable)

data IntermediateParam = IPF1 (String, NReal) | IPF2 (String, Vector2) | IPF3 (String, Vector3) | IPF4 (String, Vector4) | IPTexture (String, String, TextureWrapS, TextureWrapT) deriving Show

instance Resource Material where 
    load rm rs = do
        (m,_) <- runLoad (ld rm) rs
        return m
        
    release m = return () --TODO: IMPL
        
ld :: (MonadIO m, ResourceStream rs) => ResourceManager -> Load rs m Material
ld rm = do 
    len <- remaining
    buf <- consume len
    case (parseOnly (many pparam) buf) of 
        Left pe -> error $ show pe
        Right px -> createMaterial rm px
      
createMaterial :: MonadIO m => ResourceManager -> [IntermediateParam] -> m Material
createMaterial rm iparams = do 
    mparams <- liftIO $ mapM (toMP rm) iparams
    return $ Material { params = Map.fromList mparams }
    where 
    toMP m (IPF1 (n, v)) = return (n, MPF1 v)
    toMP m (IPF2 (n, v)) = return (n, MPF2 v)
    toMP m (IPF3 (n, v)) = return (n, MPF3 v)
    toMP m (IPF4 (n, v)) = return (n, MPF4 v)
    toMP m (IPTexture (n, p, ws, wt)) = do 
        mbin <- obtainResourceInternal p rm
        case mbin of 
            Nothing -> error $ "Could not load texture " ++ p
            Just (Binary buf) -> do
                tex <- loadTexture buf ws wt
                return (n, MPTexture tex)
        
pstr :: String -> Parser String
pstr cx = mapM char cx

pcomponent :: Parser NReal
pcomponent = do 
    c <- double
    (space >> return c) <|> return c

peolm :: Parser ()
peolm = (endOfLine >> return ()) <|> (return ())

pf1 :: Parser IntermediateParam
pf1 = do 
    pstr "f1 "
    n <- fmap B.unpack $ takeTill (\c -> c == ' ')
    char ' '
    v <- pcomponent
    peolm
    return $ IPF1 (n,v)

pf2 :: Parser IntermediateParam
pf2 = do 
    pstr "f2 "
    n <- fmap B.unpack $ takeTill (\c -> c == ' ')
    char ' '
    vec <- V2 <$> pcomponent <*> pcomponent
    peolm
    return $ IPF2 (n, vec)

pf3 :: Parser IntermediateParam
pf3 = do 
    pstr "f3 "
    n <- fmap B.unpack $ takeTill (\c -> c == ' ')
    char ' '
    vec <- V3 <$> pcomponent <*> pcomponent <*> pcomponent
    peolm
    return $ IPF3 (n, vec)

pf4 :: Parser IntermediateParam
pf4 = do 
    pstr "f4 "
    n <- fmap B.unpack $ takeTill (\c -> c == ' ')
    char ' '
    vec <- V4 <$> pcomponent <*> pcomponent <*> pcomponent <*> pcomponent
    peolm
    return $ IPF4 (n, vec)

pwrap :: Parser TextureWrap
pwrap = (pstr "clamp" >> return WrapClamp) <|> (pstr "repeat" >> return WrapRepeat)

ptexture :: Parser IntermediateParam
ptexture = do 
    pstr "tex "
    n <- fmap B.unpack $ takeTill (\c -> c == ' ')
    char ' '
    p <- fmap B.unpack $ takeTill (\c -> c == ' ' || c == '\r' || c == '\n')
    char ' '
    ws <- pwrap
    char ' '
    wt <- pwrap
    peolm
    return $ IPTexture (n, p, ws, wt)

pparam :: Parser IntermediateParam
pparam = pf1 <|> pf2 <|> pf3 <|> pf4 <|> ptexture
        
toGLWrap :: TextureWrap -> GL.GLenum
toGLWrap WrapClamp = GL.gl_CLAMP_TO_EDGE
toGLWrap WrapRepeat = GL.gl_REPEAT

getImageData :: JP.DynamicImage -> IO (Either String (GL.GLint, GL.GLint, Ptr Word8))
getImageData (JP.ImageRGB8 (JP.Image w h buf)) = V.unsafeWith buf $ \ptr -> return (Right (fromIntegral w, fromIntegral h, ptr))
getImageData (JP.ImageYCbCr8 img) = let (JP.Image w h dat) = JT.convertImage img :: JP.Image JP.PixelRGB8 in V.unsafeWith dat $ \ptr -> return (Right (fromIntegral w, fromIntegral h, ptr))
getImageData (JP.ImageCMYK8 img) = let (JP.Image w h dat) = JT.convertImage img :: JP.Image JP.PixelRGB8 in V.unsafeWith dat $ \ptr -> return (Right (fromIntegral w, fromIntegral h, ptr))
getImageData _ = return $ Left "Unsupported image format"

loadImage :: BS.ByteString -> IO (Either String (GL.GLint, GL.GLint, Ptr Word8))
loadImage buf = 
    case (JP.decodeImage buf) of 
        Left e -> return $ Left e
        Right img -> getImageData img
        
loadTexture :: BS.ByteString -> TextureWrapS -> TextureWrapT -> IO Texture
loadTexture buf ws wt = do 
    mimg <- loadImage buf 
    case mimg of 
        Left e -> error e
        Right (w, h, dta) -> do
            tid <- onPtr $ GL.glGenTextures 1
            GL.glBindTexture GL.gl_TEXTURE_2D tid
            GL.glTexImage2D GL.gl_TEXTURE_2D 0 (fromIntegral GL.gl_RGB) w h 0 (fromIntegral GL.gl_RGB) GL.gl_UNSIGNED_BYTE dta
            GL.glGenerateMipmap GL.gl_TEXTURE_2D
            GL.glTexParameteri GL.gl_TEXTURE_2D GL.gl_TEXTURE_MIN_FILTER (fromIntegral GL.gl_LINEAR_MIPMAP_LINEAR)
            GL.glTexParameteri GL.gl_TEXTURE_2D GL.gl_TEXTURE_MAG_FILTER (fromIntegral GL.gl_LINEAR)
            GL.glTexParameteri GL.gl_TEXTURE_2D GL.gl_TEXTURE_WRAP_S (fromIntegral $ toGLWrap ws)
            GL.glTexParameteri GL.gl_TEXTURE_2D GL.gl_TEXTURE_WRAP_T (fromIntegral $ toGLWrap wt)
            GL.glBindTexture GL.gl_TEXTURE_2D 0
            err <- GL.glGetError
            if (err == GL.gl_NO_ERROR) then 
                return $ Texture2D tid
            else 
                error $ "Failed to load texture: " ++ show err