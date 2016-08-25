{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
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
  , prependToPath
  , findOrInstallPulp
  , shV
  , shS
  ) where

import           Control.Exception (SomeException)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Configurator as Cfg
import           Data.Configurator.Types
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.String.Conv
import qualified Data.Text as T
import           Shelly ( run, run_, echo, fromText, errExit, catchany_sh, verbosely, lastExitCode, Sh, setenv
                        , get_env_text, shelly, escaping, toTextWarn, silently
                        )
import qualified Shelly as Sh
import           Snap
import           Snap.Snaplet.PureScript.Hooks (Hooks)
import           Text.Read hiding (String)

--------------------------------------------------------------------------------
data CompilationMode = CompileOnce
                     | CompileAlways
                     | CompileNever
                     deriving (Show, Eq, Read)

instance Configured CompilationMode where
  convert (String t) = readMaybe . T.unpack $ t
  convert _ = Nothing

--------------------------------------------------------------------------------
newtype PulpPath = PulpPath { getPulpPath :: FilePath } deriving (Show, Read)

instance Configured PulpPath where
  convert (String "") = Nothing
  convert (String t)  = Just . PulpPath . toS $ t
  convert _ = Nothing

--------------------------------------------------------------------------------
data Verbosity = Verbose
               | Quiet
               deriving (Show, Read, Eq)

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
  -- ^ The name for your bundled output.
  , pursPulpPath :: !PulpPath
  -- ^ The absolute path to a `pulp` executable. This can be user-provided
  -- or inferred automatically by this snaplet.
  , pursPsPath :: !T.Text
  -- ^ The absolute path to the directory containing the PureScript toolchain.
  -- If not specified, this snaplet will use the globally installed PureScript.
  , pursPsaOpts :: [T.Text]
  -- ^ Extra options to pass to https://github.com/natefaubion/purescript-psa,
  -- if available.
  , pursPermissiveInit :: !Bool
  -- ^ Be lenient towards compilation errors in case the `pursInit` function
  -- initial compilation fails. Useful in devel mode to avoid your web server
  -- to not start at all when you are debugging your PS.
  , pursPwdDir :: !T.Text
  -- ^ The PWD of your snaplet
  , pursOutputDir :: !T.Text
  , pursModules :: ![T.Text]
  -- ^ Where to store compilation artifacts (defaults to /js)
  , pursHooks :: Hooks
  -- ^ Hooks to run at different times during the program execution
  } deriving Show

--------------------------------------------------------------------------------
devFlagEnabled :: Bool
devFlagEnabled =
#ifdef DEVELOPMENT
  True
#else
  False
#endif

--------------------------------------------------------------------------------
shS :: MonadIO m => Sh a -> m a
shS = liftIO . shelly . silently . escaping False

--------------------------------------------------------------------------------
shV :: MonadIO m => Sh a -> m a
shV = liftIO . shelly . verbosely . escaping False

--------------------------------------------------------------------------------
findOrInstallPulp :: T.Text
                  -> Maybe PulpPath
                  -> (Monad (m b v), MonadIO (m b v), MonadSnaplet m)
                  => m b v PulpPath
findOrInstallPulp psPath mbP = do
  let p = fromMaybe (PulpPath "pulp") mbP
  installed <- shS (pulpInstalled psPath p)
  case installed of
    True  -> return p
    False -> shS $ do
      echo "Pulp not found, installing it locally for you..."
      installPulp >> whichPulp

--------------------------------------------------------------------------------
whichPulp :: MonadIO m => m PulpPath
whichPulp = PulpPath . toS . T.strip <$> shS (run "which" ["pulp"])

--------------------------------------------------------------------------------
-- | add the filepath onto the PATH env variable
prependToPath :: Sh.FilePath -> Sh ()
prependToPath fp = do
  tp <- toTextWarn fp
  pe <- get_env_text "PATH"
  setenv "PATH" $ tp <> T.singleton ':' <> pe

--------------------------------------------------------------------------------
installPulp :: MonadIO m => m ()
installPulp = shS $ run_ "npm" ["install", "pulp"]

--------------------------------------------------------------------------------
pulpInstalled :: T.Text -> PulpPath -> Sh Bool
pulpInstalled psPath (PulpPath pp) = errExit False $ verbosely $ do
  check `catchany_sh` \(e :: SomeException) -> do
    echo (toS . show $ e)
    return False
  where
    check = do
      prependToPath (fromText psPath)
      run_ (fromString pp) ["--version"]
      eC <- lastExitCode
      return $ case eC of
          0  -> True
          1  -> True
          _  -> False

--------------------------------------------------------------------------------
-- | Returns the `CompilationMode` the Snaplet should be using.
-- It will default to `CompileAlways` if your Snap app was compiled with
-- -fdevelopment or the environment is "devel", `CompileOnce` otherwise.
-- Consider using `CompileNever` if you do not want this snaplet to build
-- your .js bundle on the fly, for example if you have frozen its content and
-- you want to serve it straigth away in production.
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
