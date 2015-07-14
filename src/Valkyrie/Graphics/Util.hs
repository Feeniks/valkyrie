
module Valkyrie.Graphics.Util where 

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL.Raw as GL

onPtr :: Storable b => (Ptr b -> IO a) -> IO b
onPtr f = alloca (\p -> f p >> peek p)

packUtf8 :: String -> B.ByteString
packUtf8 = TE.encodeUtf8 . T.pack

withByteString :: B.ByteString -> (Ptr GL.GLchar -> GL.GLsizei -> IO b) -> IO b
withByteString bs act =
   BU.unsafeUseAsCStringLen bs $ \(ptr, size) ->
      act (castPtr ptr) (fromIntegral size)

withGLstring :: String -> (Ptr GL.GLchar -> IO a) -> IO a
withGLstring s act = withByteString (packUtf8 (s ++ "\0")) (const . act)
