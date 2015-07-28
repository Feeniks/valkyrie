{-# LANGUAGE OverloadedStrings #-}

module Valkyrie.Render(
    createRenderWorld, 
    getFrameBufferSize,
    setViewMatrix, 
    setProjectionMatrix,
    useModel,
    removeModel,
    render
) where 

import Valkyrie.Types
import Valkyrie.Render.Types
import Valkyrie.Valkyrie
import Valkyrie.Timer
import Valkyrie.Timer.Types
import Valkyrie.Resource
import Valkyrie.Graphics.Util
import Valkyrie.Graphics.Program
import Valkyrie.Graphics.Mesh
import Valkyrie.Graphics.Material
import Valkyrie.Graphics.Model
import Valkyrie.Math

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Map as M
import Data.List (nub)
import Foreign
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Graphics.UI.GLFW as GLFW

createRenderWorld :: C.Config -> IO RenderWorld
createRenderWorld cfg = do 
    width <- C.lookupDefault 1024 cfg "renderWidth"
    height <- C.lookupDefault 768 cfg "renderHeight"
    let view = lookAt (V3 5 5 5) (V3 0 0 0) (V3 0 1 0)
    let proj = perspective (Degrees 60) (width / height) 1.0 1000.0
    liftIO $ initRenderer
    return $ RenderWorld { _rwView = view, _rwProj = proj, _rwModels = [] }
    
getFrameBufferSize :: ValkyrieM IO (Int, Int)
getFrameBufferSize = do 
    win <- fmap _valkWindow get
    s <- liftIO $ GLFW.getFramebufferSize win
    return s
    
setViewMatrix :: Matrix44 -> ValkyrieM IO ()
setViewMatrix m = modify $ return . set (valkRenderWorld.rwView) m

setProjectionMatrix :: Matrix44 -> ValkyrieM IO ()
setProjectionMatrix m = modify $ return . set (valkRenderWorld.rwProj) m

useModel :: Model -> ValkyrieM IO ()
useModel m = modify $ return . over (valkRenderWorld.rwModels) (nub . (++[m]))

removeModel :: Model -> ValkyrieM IO ()
removeModel m = modify $ return . over (valkRenderWorld.rwModels) (filter (/=m))

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
    t <- fmap (view elapsedSeconds) timer
    GL.glClear GL.gl_COLOR_BUFFER_BIT
    GL.glClear GL.gl_DEPTH_BUFFER_BIT
    --test rendering stuff
    (Just p) <- obtainResource "data/valkyrie.prog"
    useProgram p
    bindMatrix44 p "VP" $ (world^.rwView) <::> (world^.rwProj)
    mapM_ (drawModel p world) $ world ^. rwModels
    
drawModel :: MonadIO m => Program -> RenderWorld -> Model -> m ()
drawModel p world mod = do 
    ins <- getInstances mod
    let mesh = mod^.modelMesh
    bindMeshVBO mesh
    bindMeshAttrs p
    mapM_ (uncurry $ drawPart p mesh ins) $ mod^.modelMaterials
    unbindMeshAttrs p
    
drawPart :: MonadIO m => Program -> Mesh -> [Matrix44] -> String -> Material -> m ()
drawPart p mesh ins n mat = do 
    bindMaterial p mat
    bindMeshPart n mesh
    mapM_ (\i -> bindMatrix44 p "M" i >> drawMeshPart n mesh) ins
    
bindMeshAttrs :: MonadIO m => Program -> m ()
bindMeshAttrs p = do 
    [vloc, nloc, tloc] <- mapM (getAttribLocation p) ["vPosition", "vNormal", "vTexCoord"]
    let stride = 8 * csize
    enableVertexAttrib (fromIntegral vloc) 0 stride 3
    enableVertexAttrib (fromIntegral nloc) (3 * csize) stride 3
    enableVertexAttrib (fromIntegral tloc) (6 * csize) stride 2
    
unbindMeshAttrs :: MonadIO m => Program -> m ()
unbindMeshAttrs p = do 
    [vloc, nloc, tloc] <- mapM (getAttribLocation p) ["vPosition", "vNormal", "vTexCoord"]
    GL.glDisableVertexAttribArray $ fromIntegral vloc
    GL.glDisableVertexAttribArray $ fromIntegral nloc
    GL.glDisableVertexAttribArray $ fromIntegral tloc
    
bindMaterial :: MonadIO m => Program -> Material -> m ()
bindMaterial p mat = mapM_ (uncurry $ bindMaterialParam p) $ M.toList $ params mat

bindMaterialParam :: MonadIO m => Program -> String -> MaterialParam -> m ()
bindMaterialParam p k (MPF1 v) = bindFloat1 p k v
bindMaterialParam p k (MPF2 v) = bindFloat2 p k v
bindMaterialParam p k (MPF3 v) = bindFloat3 p k v
bindMaterialParam p k (MPF4 v) = bindFloat4 p k v
bindMaterialParam p k (MPTexture v) = bindTexture p k v

enableVertexAttrib :: MonadIO m => GL.GLuint -> Int -> Int -> Int -> m ()
enableVertexAttrib index offset stride nc = do 
    GL.glEnableVertexAttribArray index
    GL.glVertexAttribPointer index (fromIntegral nc) GL.gl_FLOAT (fromBool False) (fromIntegral stride) $ plusPtr nullPtr offset

csize :: Int
csize = sizeOf (undefined :: GL.GLfloat)
