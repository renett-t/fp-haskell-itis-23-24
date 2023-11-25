{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MyLib where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import System.Directory
import System.Environment
import System.IO
import Control.Monad (forM_)

data AppConfig = AppConfig
  { 
  	depth :: Int,
    humanReadable :: Bool,
    verbose :: Bool
  }

data AppState = AppState
  { 
  	totalSpace :: Integer
  } deriving (Show)

newtype DiskUsageM a = DiskUsageM
  { 
  	unDuM :: ReaderT AppConfig (ExceptT String (WriterT String (StateT AppState IO))) a
  }
  deriving ( Functor,
             Applicative,
             Monad,
             MonadIO,
             MonadReader AppConfig,
             MonadError String,
             MonadWriter String,
             MonadState AppState
           )

runDuM :: DiskUsageM a -> AppConfig -> AppState -> IO (Either String a, String, AppState)
runDuM action config state =
  do
    ((result, logOutput), finalState) <-
      runStateT (runWriterT (runExceptT (runReaderT (unDuM action) config))) state
    return (result, logOutput, finalState)

du :: FilePath -> DiskUsageM ()
du path = do
  config <- ask
  isDir <- liftIO $ doesDirectoryExist path
  when (verbose config) $
    liftIO $ putStrLn $ "Checking: " ++ path
  if isDir
    then do
      contents <- liftIO $ listDirectory path
      forM_ contents $ \name -> do
        let newPath = path ++ "/" ++ name
        when (verbose config) $
          liftIO $ putStrLn $ "  Subdirectory: " ++ newPath
        isDir <- liftIO $ doesDirectoryExist newPath
        if isDir
          then do
            modify (\s -> s {totalSpace = totalSpace s + 1})
            du newPath
          else do
            fileSize <- liftIO $ getFilesSize newPath
            modify (\s -> s {totalSpace = totalSpace s + fromIntegral fileSize})
    else throwError $ path ++ " is not a directory"

getFilesSize :: FilePath -> IO Integer
getFilesSize path = withFile path ReadMode hFileSize
