{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.PureScript
    ( initPurs
    , pursServe
    , module Internals
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Data.Char
import           Data.Configurator as Cfg
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Paths_snaplet_purescript
import           Prelude hiding (FilePath)
import           Shelly hiding (get)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PureScript.Internal
import           Snap.Snaplet.PureScript.Internal as Internals (PureScript)
import           Text.Printf
import           Text.RawString.QQ


--------------------------------------------------------------------------------
-- | Snaplet initialization
initPurs :: SnapletInit b PureScript
initPurs = makeSnaplet "purs" description (Just dataDir) $ do
  config <- getSnapletUserConfig
  env <- getEnvironment
  outDir <- liftIO (lookupDefault "js" config "buildDir")
  verbosity   <- liftIO (lookupDefault Verbose config "verbosity")
  bndl        <- liftIO (lookupDefault True config "bundle")
  bundleName  <- liftIO (lookupDefault "app.js" config "bundleName")
  modules     <- liftIO (lookupDefault mempty config  "modules")
  pulpPath    <- findOrInstallPulp =<< liftIO (Cfg.lookup config "pulpPath")
  cm  <- getCompilationFlavour
  destDir    <- getDestDir
  bowerfile  <- fromText <$> getBowerFile
  let envCfg = fromText $ destDir <> T.pack ("/" <> env <> ".cfg")
  -- If they do not exist, create the required directories
  purs <- shelly $ verbosely $ do
    mapM_ mkdir_p [fromText outDir]
    bowerFileExists <- test_f bowerfile
    envCfgExists <- test_f envCfg
    echo $ "Checking existance of " <> toTextIgnore bowerfile
    unless bowerFileExists $ do
      chdir (fromText destDir) $ run_ "pulp" ["init"]
    echo $ "Checking existance of " <> toTextIgnore envCfg

    let purs = PureScript {
               pursCompilationMode = cm
             , pursVerbosity  = verbosity
             , pursBundle     = bndl
             , pursBundleName = bundleName
             , pursPulpPath = pulpPath
             , pursPwdDir = destDir
             , pursOutputDir = outDir
             , pursModules = modules
             }

    unless envCfgExists $ do
      touchfile envCfg
      writefile envCfg (envCfgTemplate purs)
    return purs

  -- compile at least once, regardless of the CompilationMode.
  -- NOTE: We might want to ignore the ouput of this first compilation
  -- if we are running in a 'permissive' mode, to avoid having the entire
  -- web service to grind to an halt in case our Purs does not compile.
  res <- build  purs
  _   <- bundle purs
  case res of
    CompilationFailed reason -> fail (T.unpack reason)
    CompilationSucceeded -> return ()

  return purs
  where
    description = "Automatic (re)compilation of PureScript projects"
    dataDir = liftM (++ "/resources") getDataDir

--------------------------------------------------------------------------------
pursLog :: String -> Handler b PureScript ()
pursLog l = do
  verb <- get >>= return . pursVerbosity
  unless (verb == Quiet) (liftIO $ putStrLn $ "snaplet-purescript: " <> l)

--------------------------------------------------------------------------------
pursServe :: Handler b PureScript ()
pursServe = do
  (_, requestedJs) <- (fmap (T.takeWhile (/= '?')) . T.breakOn "/" . T.drop 1 . TE.decodeUtf8 . rqURI) <$> getRequest
  case requestedJs of
    "" -> fail (jsNotFound requestedJs)
    _ -> do
      pursLog $ "Requested file: " <> T.unpack requestedJs
      modifyResponse . setContentType $ "text/javascript;charset=utf-8"
      outDir <- getAbsoluteOutputDir
      let fulljsPath = outDir <> requestedJs
      pursLog $ "Serving " <> T.unpack (fulljsPath)
      res <- compileWithMode
      _   <- bundleWithMode (T.drop 1 requestedJs)
      case res of
          CompilationFailed reason -> writeText reason
          CompilationSucceeded ->
            (shelly $ silently $ readfile (fromText fulljsPath)) >>= writeText

--------------------------------------------------------------------------------
-- | Build the project (without bundling it).
build :: MonadIO m => PureScript -> m CompilationOutput
build PureScript{..} =
  liftIO $ shelly $ verbosely $ errExit False $
    chdir (fromText pursPwdDir) $ do
      res <- run "pulp" ["build", "-o", pursOutputDir]
      eC <- lastExitCode
      case (eC == 0) of
          True -> return CompilationSucceeded
          False -> return $ CompilationFailed res

--------------------------------------------------------------------------------
bundle :: MonadIO m => PureScript -> m CompilationOutput
bundle PureScript{..} =
  liftIO $ shelly $ verbosely $ errExit False $ chdir (fromText pursPwdDir) $ do
      let bundlePath = pursOutputDir <> "/" <> pursBundleName
      case pursBundle of
        False -> return CompilationSucceeded
        True -> do
          rm_rf (fromText bundlePath)
          let modules = T.intercalate " -m " pursModules
          echo $ "Bundling everything in " <> bundlePath
          res <- run "psc-bundle" (["js/**/*.js", "-m"] <> (T.words modules) <>
                                   ["-o", bundlePath, "-n", "PS"])
          eC <- lastExitCode
          case (eC == 0) of
            True -> return CompilationSucceeded
            False -> return $ CompilationFailed res

--------------------------------------------------------------------------------
compileWithMode :: Handler b PureScript CompilationOutput
compileWithMode = do
  mode <- gets pursCompilationMode
  case mode of
    CompileOnce -> return CompilationSucceeded
    CompileAlways -> do
      workDir <- gets pursPwdDir
      pursLog $ "Compiling Purescript project at " <> T.unpack workDir
      get >>= build

--------------------------------------------------------------------------------
bundleWithMode :: T.Text -> Handler b PureScript CompilationOutput
bundleWithMode _ = do
  mode <- gets pursCompilationMode
  case mode of
    CompileOnce -> return CompilationSucceeded
    CompileAlways -> do
      workDir <- gets pursPwdDir
      pursLog $ "Bundling Purescript project at " <> T.unpack workDir
      get >>= bundle

--------------------------------------------------------------------------------
jsNotFound :: T.Text -> String
jsNotFound js = printf [r|
You asked me to serve:

%s

But I wasn't able to find a suitable PureScript module to build.

If this is the first time you are running snaplet-purescript, have
a look inside snaplets/purs/devel.cfg.
|] (T.unpack js)

--------------------------------------------------------------------------------
envCfgTemplate :: PureScript -> T.Text
envCfgTemplate PureScript{..} = T.pack $ printf [r|
  # Choose one between 'Verbose' and 'Quiet'
  #
  verbosity = "%s"
  #
  # Choose one between 'CompileOnce' and 'CompileAlways'
  #
  compilationMode = "%s"
  #
  # Whether bundle everything in a fat app
  #
  bundle     = %s
  #
  # The path to a specific, user-provided version of Pulp.
  # Leave it uncommented if you plan to use the globally-installed one or you
  # are OK with snaplet-purescript installing it for you.
  #
  # pulpPath = ""
  #
  # The name of the output bundle
  bundleName = "%s"
  #
  # The list of modules you want to compile under the PS namespace (bundle only)
  modules = []
|] (show pursVerbosity)
   (show pursCompilationMode)
   (map toLower $ show pursBundle)
   (T.unpack pursBundleName)
