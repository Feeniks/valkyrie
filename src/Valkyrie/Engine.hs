{-# LANGUAGE OverloadedStrings #-}

module Valkyrie.Engine(
    valkyrie,
    exitValkyrie
) where 

import Valkyrie.Types
import Valkyrie.Valkyrie
import Valkyrie.Game
import Valkyrie.Timer
import Valkyrie.Resource
import Valkyrie.Resource.File
import Valkyrie.Render

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Graphics.UI.GLFW as GLFW
import Prelude hiding (init)

valkyrie :: Game g => g -> IO ()
valkyrie g = do 
    valk <- createValkyrie
    runValkyrieM (init g >>= mainLoop >>= term) valk
    return ()
    
exitValkyrie :: Monad m => ValkyrieM m ()
exitValkyrie = modify $ return . (set valkExit True)
    
createValkyrie :: IO Valkyrie
createValkyrie = do 
    cfg <- C.load [C.Optional "valkyrie.cfg"]
    win <- createWindow cfg
    tmr <- createTimer
    let fileLocator = createFileResourceLocator "./"
    resMgr <- fmap (attachLocator fileLocator) createResourceManager
    renderWorld <- createRenderWorld cfg
    return $ Valkyrie {
        _valkCfg = cfg,
        _valkWindow = win,
        _valkExit = False,
        _valkTimer = tmr, 
        _valkResourceManager = resMgr,
        _valkRenderWorld = renderWorld
    }
    
mainLoop :: Game g => g -> ValkyrieM IO g
mainLoop g = do
    updateTimer
    win <- fmap _valkWindow get
    liftIO $ GLFW.pollEvents
    render
    --TODO: physics etc
    liftIO $ GLFW.swapBuffers win
    g' <- tick g
    ex <- fmap _valkExit get
    case ex of 
        True -> return g'
        False -> mainLoop g'
        
term :: Game g => g -> ValkyrieM IO ()
term g = do 
    shutdown g
    valk <- get
    liftIO $ destroyWindow $ _valkWindow valk

createWindow :: C.Config -> IO GLFW.Window
createWindow cfg = do 
    width <- C.lookupDefault 1024 cfg "renderWidth"
    height <- C.lookupDefault 768 cfg "renderHeight"
    title <- C.lookupDefault "Valkyrie" cfg "windowTitle"
    GLFW.init
    mwin <- GLFW.createWindow width height title Nothing Nothing
    case mwin of 
        Nothing -> error "Failed to create Valkyrie render window"
        Just win -> do 
            GLFW.makeContextCurrent mwin
            return win
            
destroyWindow :: GLFW.Window -> IO ()
destroyWindow = GLFW.destroyWindow
