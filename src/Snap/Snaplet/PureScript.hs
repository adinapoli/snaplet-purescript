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
import           Data.Configurator as Cfg
import           Data.Monoid
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Paths_snaplet_purescript
import           Prelude hiding (FilePath)
import           Shelly hiding (get)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PureScript.Hooks
import           Snap.Snaplet.PureScript.Internal as I
import           Snap.Snaplet.PureScript.Internal as Internals (PureScript)
import           Text.Printf
import           Text.RawString.QQ


--------------------------------------------------------------------------------
-- | Snaplet initialization
initPurs :: SnapletInit b PureScript
initPurs = makeSnaplet "purs" description (Just dataDir) $ do
  env <- getEnvironment

  destDir    <- getDestDir
  shelly $ verbosely $ chdir (fromText destDir) $ do
    let envCfg = fromText $ destDir <> T.pack ("/" <> env <> ".cfg")
    echo $ "Checking existance of " <> toTextIgnore envCfg
    envCfgExists <- test_f envCfg
    unless envCfgExists $ do
      touchfile envCfg
      writefile envCfg envCfgTemplate

  config <- getSnapletUserConfig
  hooks  <- liftIO $ getHooks config
  shelly $ verbosely $ chdir (fromText destDir) $ preInitHook hooks

  outDir <- liftIO (lookupDefault "js" config "buildDir")
  verbosity   <- liftIO (lookupDefault Verbose config "verbosity")
  bndl        <- liftIO (lookupDefault True config "bundle")
  bundleName  <- liftIO (lookupDefault "app.js" config "bundleName")
  bundleExe   <- liftIO (lookupDefault "psc-bundle" config "bundleExe")
  bundleOpts  <- liftIO (lookupDefault mempty config "bundleOpts")
  modules     <- liftIO (lookupDefault ["Main"] config  "modules")
  psPath      <- liftIO (lookupDefault mempty config "pureScriptPath")
  pulpPath    <- findOrInstallPulp psPath =<< liftIO (Cfg.lookup config "pulpPath")
  psaOpts     <- liftIO (lookupDefault mempty config "psaOpts")
  permissive  <- liftIO (lookupDefault False config "permissiveInit")
  cm  <- getCompilationFlavour
  bowerfile  <- fromText <$> getBowerFile
  -- If they do not exist, create the required directories
  purs <- shelly $ verbosely $ chdir (fromText destDir) $ do
    mkdir_p (fromText outDir)
    bowerFileExists <- test_f bowerfile
    echo $ "Checking existance of " <> toTextIgnore bowerfile
    unless bowerFileExists $ do
      run_ (fromString $ getPulpPath pulpPath) ["init"]

    return PureScript {
             pursCompilationMode = cm
           , pursVerbosity       = verbosity
           , pursBundle          = bndl
           , pursBundleName      = bundleName
           , pursBundleExe       = bundleExe
           , pursBundleOpts      = bundleOpts
           , pursPulpPath        = pulpPath
           , pursPsPath          = psPath
           , pursPsaOpts         = psaOpts
           , pursPermissiveInit  = permissive
           , pursPwdDir          = destDir
           , pursOutputDir       = outDir
           , pursModules         = modules
           , pursHooks           = hooks
           }

  -- compile at least once, unless `CompileNever` was passed.
  -- NOTE: We ignore the ouput of this first compilation
  -- if we are running in a 'permissive' mode, to avoid having the entire
  -- web service to grind to an halt in case our Purs does not compile.
  unless (cm == CompileNever) $ do
    res <- build  purs
    _   <- bundle purs
    case res of
      CompilationFailed reason -> unless permissive (fail (T.unpack reason))
      CompilationSucceeded -> return ()

  shelly $ verbosely $ chdir (fromText destDir) $ postInitHook hooks
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
          CompilationFailed reason -> do
            let curatedOutput = T.replace "\"" "\\\"" . T.replace "\n" "\\n" $ reason
            writeText $ "(function() { console.log(\"" <> curatedOutput <> "\"); })();"
          CompilationSucceeded ->
            (shelly $ silently $ readfile (fromText fulljsPath)) >>= writeText

--------------------------------------------------------------------------------
-- | Build the project (without bundling it).
build :: MonadIO m => PureScript -> m CompilationOutput
build PureScript{..} = shV $ errExit False $ do
  chdir (fromText pursPwdDir) $ do
    I.prependToPath (fromText pursPsPath)
    preBuildHook pursHooks
    let args = ["build", "-o", pursPwdDir <> "/" <> pursOutputDir] <> pursPsaOpts
    run_ (fromString . getPulpPath $ pursPulpPath) args
    eC <- lastExitCode
    preBuildHook pursHooks
    case (eC == 0) of
        True  -> return CompilationSucceeded
        False -> CompilationFailed <$> lastStderr

--------------------------------------------------------------------------------
bundle :: MonadIO m => PureScript -> m CompilationOutput
bundle PureScript{..} =
  liftIO $ shelly $ verbosely $ errExit False $ chdir (fromText pursPwdDir) $ do
      I.prependToPath (fromText pursPsPath)
      let bundlePath = pursOutputDir <> "/" <> pursBundleName
      case pursBundle of
        False -> return CompilationSucceeded
        True -> do
          preBundleHook pursHooks
          rm_rf (fromText bundlePath)
          echo $ "Bundling everything in " <> bundlePath
          res <- case (pursBundleExe, pursBundleOpts) of
                ("psc-bundle", []) ->
                  let modules = T.intercalate " -m " pursModules
                  in run "psc-bundle" (["js/**/*.js", "-m"] <> (T.words modules) <> ["-o", bundlePath, "-n", "PS"])
                ("pulp", [])       ->
                 let modules = T.intercalate "," pursModules
                 in run "pulp" (["build", "-I", "src", "--modules"] <> (T.words modules) <> ["-t", bundlePath])
                (exe, args)        -> run (fromText exe) args
          postBundleHook pursHooks
          eC <- lastExitCode
          case (eC == 0) of
            True -> return CompilationSucceeded
            False -> return $ CompilationFailed res

--------------------------------------------------------------------------------
compileWithMode :: Handler b PureScript CompilationOutput
compileWithMode = do
  mode <- gets pursCompilationMode
  case mode of
    CompileNever -> return CompilationSucceeded
    CompileOnce  -> return CompilationSucceeded
    CompileAlways -> do
      workDir <- gets pursPwdDir
      pursLog $ "Compiling Purescript project at " <> T.unpack workDir
      get >>= build

--------------------------------------------------------------------------------
bundleWithMode :: T.Text -> Handler b PureScript CompilationOutput
bundleWithMode _ = do
  mode <- gets pursCompilationMode
  case mode of
    CompileOnce  -> return CompilationSucceeded
    CompileNever -> return CompilationSucceeded
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
envCfgTemplate :: T.Text
envCfgTemplate = T.pack [r|
# Choose one between 'Verbose' and 'Quiet'
#
verbosity = "Verbose"
#
# Choose one between 'CompileOnce' and 'CompileAlways'
#
compilationMode = "CompileAlways"
#
# Whether bundle everything in a fat app
#
bundle     = true
#
# Override the bundle command executable
#
bundleExe  = "pulp"
#
# Override the bundle command arguments
#
bundleOpts = []
#
# The path to a specific directory containing the purescript toolchain.
# Example: snaplet/purs/node_modules/purescript/vendor.
# Leave it uncommented if you plan to use the globally-installed one.
#
# pureScriptPath = ""
#
# The path to a specific, user-provided version of Pulp.
# Leave it uncommented if you plan to use the globally-installed one or you
# are OK with snaplet-purescript installing it for you.
#
# pulpPath = ""
#
# Extra options to pass to https://github.com/natefaubion/purescript-psa,
# if available.
psaOpts = []
#
permissiveInit = false
# Be lenient towards compilation errors in case the `pursInit` function
# initial compilation fails. Useful in devel mode to avoid your web server
# to not start at all when you are debugging your PS.
#
# The name of the output bundle
bundleName = "app.js"
#
# The list of modules you want to compile under the PS namespace (bundle only)
# Adding 'Main' will make sure you will have something like PS.Main.main in
# your generated JS.
modules = ["Main"]
#
# Hooks - They are a way to invoke certain action during the snaplet lifecycle.
# They accept a shell command where the first token is the command itself,
# the rest are the parameters for the command. The entire hook section or each
# individual hook can be omitted.
hooks {
  preInit    = "echo 'hello'"
  postInit   = ""
  preBuild   = ""
  postBuild  = ""
  preBundle  = ""
  postBundle = ""
}
|]
