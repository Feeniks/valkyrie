
module Valkyrie.Binary(Binary(..)) where 

import Valkyrie.Resource
import Valkyrie.Resource.Types

import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import Data.Typeable

data Binary = Binary B.ByteString deriving (Show, Typeable)

instance Resource Binary where 
    load rm rs = do
        (b,_) <- runLoad ld rs
        return b
        
    release b = return ()
    
ld :: (MonadIO m, ResourceStream rs) => Load rs m Binary
ld = do 
    len <- remaining
    buf <- consume len
    return $ Binary buf