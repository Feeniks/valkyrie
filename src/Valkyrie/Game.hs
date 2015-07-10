
module Valkyrie.Game where 

import Valkyrie.Types
import Valkyrie.Valkyrie

class Game g where 
    init :: g -> ValkyrieM IO g
    tick :: g -> ValkyrieM IO g
    shutdown :: g -> ValkyrieM IO ()
