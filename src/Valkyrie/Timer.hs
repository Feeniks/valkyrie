
module Valkyrie.Timer(
    createTimer,
    updateTimer,
    timer
) where 

import Valkyrie.Types
import Valkyrie.Valkyrie
import Valkyrie.Timer.Types

import Control.Lens
import Control.Monad.Trans
import Graphics.UI.GLFW

timer :: ValkyrieM IO Timer
timer = fmap _valkTimer get

updateTimer :: ValkyrieM IO ()
updateTimer = modify $ \valk -> do 
    t <- liftIO $ _getTime
    let currentElapsed = valk ^. (valkTimer.elapsedSeconds)
    let valk' = valk & (valkTimer.elapsedSeconds) .~ t & (valkTimer.elapsedFrames) %~ (+1) & (valkTimer.frameDuration) .~ (t - currentElapsed)
    return valk'

createTimer :: IO Timer
createTimer = do 
    liftIO $ setTime 0
    t <- _getTime
    return $ Timer t 0 0
        
_getTime :: IO Double
_getTime = do 
    mt <- getTime
    case mt of 
        Nothing -> error "Could not poll timer"
        Just t -> return t
