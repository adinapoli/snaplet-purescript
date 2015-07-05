{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.PureScript.Internal where

import           Snap
import           Data.Monoid
import           Control.Monad.IO.Class
import           Data.Configurator as Cfg
import           Data.Configurator.Types
import           Text.Read hiding (String)
import qualified Data.Text as T

--------------------------------------------------------------------------------
data CompilationMode =
      CompileOnce
    | CompileAlways
    deriving (Show, Read)

instance Configured CompilationMode where
  convert (String t) = readMaybe . T.unpack $ t
  convert _ = Nothing

data Verbosity = Verbose | Quiet deriving (Show, Read, Eq)

instance Configured Verbosity where
  convert (String t) = readMaybe . T.unpack $ t
  convert _ = Nothing

--------------------------------------------------------------------------------
data CompilationOutput = CompilationFailed T.Text
                       | CompilationSucceeded
                       deriving (Show, Ord, Eq)

--------------------------------------------------------------------------------
data PureScript = PureScript {
    pursCompilationMode :: CompilationMode
  , pursVerbosity :: Verbosity
  }

--------------------------------------------------------------------------------
devFlagEnabled :: Bool
devFlagEnabled =
#ifdef DEVELOPMENT
  True
#else
  False
#endif

--------------------------------------------------------------------------------
-- | Returns the 'CompilationMode' the Snaplet should be using.
-- It will default to 'CompileAlways' if your Snap app was compiled with
-- -fdevelopment or the environment is "devel", 'CompileOnce' otherwise.
getCompilationFlavour :: Initializer b v CompilationMode
getCompilationFlavour = do
 -- Any input for the user have highest priority
 cfg <- getSnapletUserConfig
 cm <- liftIO (Cfg.lookup cfg "compilationMode")
 case cm of
  Just c -> return c
  Nothing -> do
    inDevelMode <- ("devel" ==) <$> getEnvironment
    return $ if or [inDevelMode, devFlagEnabled]
               then CompileAlways
               else CompileOnce

--------------------------------------------------------------------------------
getDestDir :: (Monad (m b v), MonadIO (m b v), MonadSnaplet m) => m b v T.Text
getDestDir = do
  fp <- getSnapletFilePath
  return $ T.pack fp

--------------------------------------------------------------------------------
getSrcDir :: (Monad (m b v), MonadIO (m b v), MonadSnaplet m) => m b v T.Text
getSrcDir = return . (`mappend` "/src") =<< getDestDir

--------------------------------------------------------------------------------
getJsDir :: (Monad (m b v), MonadIO (m b v), MonadSnaplet m) => m b v T.Text
getJsDir = return . (`mappend` "/js") =<< getDestDir

--------------------------------------------------------------------------------
getGruntfile :: (Monad (m b v), MonadIO (m b v), MonadSnaplet m) => m b v T.Text
getGruntfile = return . (`mappend` "/Gruntfile.js") =<< getDestDir
