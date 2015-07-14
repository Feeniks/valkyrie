
module Valkyrie.Render(
    createRenderWorld,
    render
) where 

import Valkyrie.Types
import Valkyrie.Render.Types
import Valkyrie.Valkyrie

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Graphics.Rendering.OpenGL.Raw as GL

createRenderWorld :: C.Config -> IO RenderWorld
createRenderWorld cfg = do 
    liftIO $ initRenderer
    return $ RenderWorld { }

render :: ValkyrieM IO ()
render = do 
    world <- fmap (view valkRenderWorld) get
    liftIO $ frame world
    
initRenderer :: IO ()
initRenderer = do 
    GL.glClearColor 0 0 0 1
    GL.glEnable GL.gl_DEPTH_TEST
    GL.glDepthFunc GL.gl_LESS
    GL.glEnable GL.gl_CULL_FACE
    GL.glCullFace GL.gl_BACK
    
frame :: RenderWorld -> IO ()
frame world = do 
    GL.glClear GL.gl_COLOR_BUFFER_BIT
    GL.glClear GL.gl_DEPTH_BUFFER_BIT
