
module Valkyrie.Input(
    Graphics.UI.GLFW.Key(..),
    Graphics.UI.GLFW.KeyState(..),
    Graphics.UI.GLFW.MouseButton(..),
    Graphics.UI.GLFW.MouseButtonState(..),
    keyState,
    mouseButtonState,
    cursorPosition
) where 

import Valkyrie.Types
import Valkyrie.Valkyrie

import Control.Monad.Trans
import Graphics.UI.GLFW

keyState :: Key -> ValkyrieM IO KeyState
keyState k = overWindow $ flip getKey k
    
mouseButtonState :: MouseButton -> ValkyrieM IO MouseButtonState
mouseButtonState b = overWindow $ flip getMouseButton b
    
cursorPosition :: ValkyrieM IO (Double, Double)
cursorPosition = overWindow $ getCursorPos
    
overWindow :: Monad m => (Window -> m a) -> ValkyrieM m a
overWindow f = fmap _valkWindow get >>= lift . f
