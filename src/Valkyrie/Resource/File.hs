
module Valkyrie.Resource.File (
    FileResourceStream, 
    FileResourceLocator, 
    createFileResourceLocator
) where

import Valkyrie.Resource.Types
import Valkyrie.Resource

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString as B
import System.Directory
import System.IO

data FileResourceStream = FileResourceStream { h :: Handle, r :: Int } deriving Show

data FileResourceLocator = FRL String

instance ResourceStream FileResourceStream where 
    remaining = Load $ \rs -> return (r rs, rs)
    consume n = Load $ \rs -> do 
        buf <- liftIO $ B.hGet (h rs) n
        return (buf, rs { r = (r rs) - B.length buf })

instance ResourceLocator FileResourceLocator where 
    resolve rm p (FRL bp) = do 
        let filepath = bp ++ p
        exists <- liftIO $ doesFileExist filepath
        case exists of 
            False -> return Nothing
            True -> do 
                rs <- fileStream filepath
                r <- load rm rs
                return $ Just r

createFileResourceLocator :: String -> FileResourceLocator
createFileResourceLocator basepath = FRL basepath

fileStream :: MonadIO m => String -> m FileResourceStream
fileStream p = do 
    handle <- liftIO $ openFile p ReadMode
    size <- liftIO (hFileSize handle)
    return $ FileResourceStream { h = handle, r = fromIntegral size }