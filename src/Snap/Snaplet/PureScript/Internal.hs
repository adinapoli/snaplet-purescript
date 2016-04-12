{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.PureScript.Internal (
    CompilationMode(..)
  , CompilationOutput(..)
  , Verbosity(..)
  , PulpPath(getPulpPath)
  , PureScript(..)
  , devFlagEnabled
  , getCompilationFlavour
  , getDestDir
  , getBowerFile
  , getAbsoluteOutputDir
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Configurator as Cfg
import           Data.Configurator.Types
import           Data.Monoid
import           Data.String.Conv
import qualified Data.Text as T
import           Snap
import           Text.Read hiding (String)

--------------------------------------------------------------------------------
data CompilationMode =
      CompileOnce
    | CompileAlways
    deriving (Show, Read)

instance Configured CompilationMode where
  convert (String t) = readMaybe . T.unpack $ t
  convert _ = Nothing

--------------------------------------------------------------------------------
newtype PulpPath = PulpPath { getPulpPath :: FilePath }
    deriving (Show, Read)

instance Configured PulpPath where
  convert (String t) = Just . PulpPath . toS $ t
  convert _ = Nothing

data Verbosity = Verbose | Quiet deriving (Show, Read, Eq)

instance Configured Verbosity where
  convert (String t) = readMaybe . T.unpack $ t
  convert _ = Nothing

--------------------------------------------------------------------------------
data CompilationOutput = CompilationFailed !T.Text
                       | CompilationSucceeded
                       deriving (Show, Ord, Eq)

--------------------------------------------------------------------------------
data PureScript = PureScript {
    pursCompilationMode :: CompilationMode
  , pursVerbosity :: Verbosity
  , pursBundle :: !Bool
  -- ^ Whether or not bundle everything in a fat app with a PS namespace.
  , pursBundleName :: !T.Text
  , pursPwdDir :: !T.Text
  -- ^ The CWD of your snaplet
  , pursOutputDir :: !T.Text
  , pursModules :: ![T.Text]
  -- ^ Where to store compilation artifacts (defaults to /js)
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
    return $ if inDevelMode || devFlagEnabled
               then CompileAlways
               else CompileOnce

--------------------------------------------------------------------------------
getDestDir :: (Monad (m b v), MonadIO (m b v), MonadSnaplet m) => m b v T.Text
getDestDir = do
  fp <- getSnapletFilePath
  return $ T.pack fp

--------------------------------------------------------------------------------
getBowerFile :: (Monad (m b v), MonadIO (m b v), MonadSnaplet m) => m b v T.Text
getBowerFile = (`mappend` "/bower.json") <$> getDestDir

--------------------------------------------------------------------------------
getAbsoluteOutputDir :: Handler b PureScript T.Text
getAbsoluteOutputDir = do
  wDir <- asks pursPwdDir
  oDir <- asks pursOutputDir
  return $ wDir <> "/" <> oDir
