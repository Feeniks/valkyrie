{-# LANGUAGE OverloadedStrings #-}

module Valkyrie.Render(
    createRenderWorld, 
    getFrameBufferSize,
    setViewMatrix, 
    setProjectionMatrix,
    render
) where 

import Valkyrie.Types
import Valkyrie.Render.Types
import Valkyrie.Valkyrie
import Valkyrie.Resource
import Valkyrie.Graphics.Util
import Valkyrie.Graphics.Program
import Valkyrie.Graphics.Mesh
import Valkyrie.Graphics.Material
import Valkyrie.Math

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Map as M
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Graphics.UI.GLFW as GLFW

createRenderWorld :: C.Config -> IO RenderWorld
createRenderWorld cfg = do 
    width <- C.lookupDefault 1024 cfg "renderWidth"
    height <- C.lookupDefault 768 cfg "renderHeight"
    let view = lookAt (V3 5 5 5) (V3 0 0 0) (V3 0 1 0)
    let proj = perspective (Degrees 60) (width / height) 1.0 1000.0
    liftIO $ initRenderer
    return $ RenderWorld { _rwView = view, _rwProj = proj }
    
getFrameBufferSize :: ValkyrieM IO (Int, Int)
getFrameBufferSize = do 
    win <- fmap _valkWindow get
    s <- liftIO $ GLFW.getFramebufferSize win
    return s
    
setViewMatrix :: Matrix44 -> ValkyrieM IO ()
setViewMatrix m = modify $ return . set (valkRenderWorld.rwView) m

setProjectionMatrix :: Matrix44 -> ValkyrieM IO ()
setProjectionMatrix m = modify $ return . set (valkRenderWorld.rwProj) m

render :: ValkyrieM IO ()
render = do 
    world <- fmap _valkRenderWorld get
    frame world
    
initRenderer :: IO ()
initRenderer = do 
    GL.glClearColor 0 0 0 1
    GL.glEnable GL.gl_DEPTH_TEST
    GL.glDepthFunc GL.gl_LESS
    GL.glEnable GL.gl_CULL_FACE
    GL.glCullFace GL.gl_BACK
    vid <- onPtr (GL.glGenVertexArrays 1)
    GL.glBindVertexArray vid
    
frame :: RenderWorld -> ValkyrieM IO ()
frame world = do 
    GL.glClear GL.gl_COLOR_BUFFER_BIT
    GL.glClear GL.gl_DEPTH_BUFFER_BIT
    --test rendering stuff
    (Just p) <- obtainResource "data/valkyrie.prog"
    (Just mesh) <- obtainResource "data/cube_no_normals.mdl" :: ValkyrieM IO (Maybe Mesh)
    (Just mat) <- obtainResource "data/t.mtl" :: ValkyrieM IO (Maybe Material)
    useProgram p
    bindMatrix44 p "M" $ identity44
    bindMatrix44 p "VP" $ (world^.rwView) <::> (world^.rwProj)
    bindMesh mesh
    bindMaterial p mat
    drawMeshPart "cube" mesh
    
bindMaterial :: MonadIO m => Program -> Material -> m ()
bindMaterial p mat = mapM_ (uncurry $ bindMaterialParam p) $ M.toList $ params mat

bindMaterialParam :: MonadIO m => Program -> String -> MaterialParam -> m ()
bindMaterialParam p k (MPF1 v) = bindFloat1 p k v
bindMaterialParam p k (MPF2 v) = bindFloat2 p k v
bindMaterialParam p k (MPF3 v) = bindFloat3 p k v
bindMaterialParam p k (MPF4 v) = bindFloat4 p k v
bindMaterialParam p k (MPTexture v) = bindTexture p k v