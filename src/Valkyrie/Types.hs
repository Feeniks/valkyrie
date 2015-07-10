{-# LANGUAGE TemplateHaskell #-}

module Valkyrie.Types where 

import Valkyrie.Timer.Types
import Valkyrie.Resource.Types

import Control.Lens.TH
import qualified Data.Configurator.Types as C
import qualified Graphics.UI.GLFW as GLFW

data Valkyrie = Valkyrie { 
    _valkCfg :: C.Config,
    _valkWindow :: GLFW.Window,
    _valkExit :: Bool,
    _valkTimer :: Timer,
    _valkResourceManager :: ResourceManager
}

makeLenses ''Valkyrie

newtype ValkyrieM m a = ValkyrieM { runValkyrieM :: Valkyrie -> m (a, Valkyrie) }