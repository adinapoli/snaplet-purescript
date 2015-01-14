{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.PureScript.Internal where

import           Snap
import           Data.Monoid
import           Data.Configurator.Types
import           Text.Read hiding (String)
import qualified Data.Text as T

--------------------------------------------------------------------------------
data CompilationMode =
      CompileOnce
    | CompileAlways
    deriving Show

data Verbosity = Verbose | Quiet deriving (Show, Read, Eq)

instance Configured Verbosity where
  convert (String t) = readMaybe . T.unpack $ t
  convert _ = Nothing

--------------------------------------------------------------------------------
data PureScript = PureScript {
    pursCompilationMode :: CompilationMode
  , pursVerbosity :: Verbosity
  }

--------------------------------------------------------------------------------
-- | TODO: This doesn't seem to work, but I do not want to 
-- spend all my time on this now.
devFlagEnabled :: Bool
devFlagEnabled =
#if defined(DEVELOPMENT)
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
