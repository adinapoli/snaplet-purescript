{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.PureScript 
    ( initPurs
    , pursServe
    , module Internals
    ) where

import           Prelude hiding (FilePath)
import           Control.Monad.IO.Class
import           Snap.Core
import           Snap.Snaplet
import           Control.Monad
import           Control.Monad.State.Strict
import           Paths_snaplet_purescript
import           Shelly hiding (get)
import           Text.Printf
import           Data.Monoid
import           Data.Configurator
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import           Snap.Snaplet.PureScript.Internal as Internals (PureScript)
import           Snap.Snaplet.PureScript.Internal
import           Text.RawString.QQ

--------------------------------------------------------------------------------
-- | Snaplet initialization
initPurs :: SnapletInit b PureScript
initPurs = makeSnaplet "purs" description (Just dataDir) $ do
  config <- getSnapletUserConfig
  env <- getEnvironment
  outDir <- liftIO (lookupDefault "js" config "buildDir")
  verbosity <- liftIO (lookupDefault Verbose config "verbosity")
  cm  <- getCompilationFlavour
  destDir    <- getDestDir
  let buildDir = destDir <> "/" <> outDir
  bowerfile  <- fromText <$> getBowerFile
  let envCfg = fromText $ destDir <> T.pack ("/" <> env <> ".cfg")
  -- If they do not exist, create the required directories
  shelly $ silently $ do
    mapM_ mkdir_p [fromText outDir]
    bowerFileExists <- test_f bowerfile
    envCfgExists <- test_f envCfg
    unlessM (pulpInstalled (fromText destDir)) $ do
      liftIO $ do
        putStrLn "Local pulp not found, installing it for you..."
        installPulp (fromText destDir)
    unless bowerFileExists $ do
      chdir (fromText destDir) $ run_ "pulp" ["init"]
    unless envCfgExists $ do
      touchfile envCfg
      writefile envCfg (envCfgTemplate verbosity cm)

  -- compile at least once, regardless of the CompilationMode.
  -- NOTE: We might want to ignore the ouput of this first compilation
  -- if we are running in a 'permissive' mode, to avoid having the entire
  -- web service to grind to an halt in case our Purs does not compile.
  res <- compileAll (fromText destDir) buildDir
  case res of
    CompilationFailed reason -> fail (T.unpack reason)
    CompilationSucceeded -> return ()

  return $ PureScript cm verbosity outDir destDir
  where
    description = "Automatic (re)compilation of PureScript projects"
    dataDir = liftM (++ "/resources") getDataDir

--------------------------------------------------------------------------------
pursLog :: String -> Handler b PureScript ()
pursLog l = do
  verb <- get >>= return . pursVerbosity
  unless (verb == Quiet) (liftIO $ putStrLn $ "snaplet-purescript: " <> l)

--------------------------------------------------------------------------------
pulpInstalled :: FilePath -> Sh Bool
pulpInstalled dir = errExit False $ chdir dir $ do
  run_ "pulp" []
  eC <- lastExitCode
  return $ case eC of
      0  -> True
      _ -> False

--------------------------------------------------------------------------------
installPulp :: MonadIO m => FilePath -> m ()
installPulp dir = liftIO $ shelly $ silently $ chdir dir $ do
 run_ "npm" ["install", "-g", "pulp"]

--------------------------------------------------------------------------------
pursServe :: Handler b PureScript ()
pursServe = do
  modifyResponse . setContentType $ "text/javascript;charset=utf-8"
  outDir <- getAbsoluteOutputDir
  pwdDir <- getDestDir
  compMode <- gets pursCompilationMode
  res <- compileWithMode compMode (fromText pwdDir) outDir
  case res of
    CompilationFailed reason -> writeText reason
    CompilationSucceeded -> do
      -- Now get the requested file and try to serve it
      -- This returns something like /purs/Hello/index.js
      (_, requestedJs) <- T.breakOn "/" . T.drop 1 . TE.decodeUtf8 . rqURI <$> getRequest
      case requestedJs of
        "" -> fail (jsNotFound requestedJs)
        _  -> do
          pursLog $ "Serving " <> T.unpack requestedJs
          outDir <- getAbsoluteOutputDir
          let fulljsPath = outDir <> requestedJs
          (shelly $ silently $ readfile (fromText fulljsPath)) >>= writeText

--------------------------------------------------------------------------------
compileAll :: MonadIO m => FilePath -> T.Text -> m CompilationOutput
compileAll workDir outputDir =
  liftIO $ shelly $ silently $ errExit False $ chdir workDir $ do
    res <- run "pulp" ["build", "-O", "--to", outputDir]
    eC <- lastExitCode
    case (eC == 0) of
        True -> return CompilationSucceeded
        False -> return $ CompilationFailed res

--------------------------------------------------------------------------------
compileWithMode :: CompilationMode
                -> FilePath
                -> T.Text
                -> Handler b PureScript CompilationOutput
compileWithMode CompileOnce _ _ = return CompilationSucceeded
compileWithMode CompileAlways workDir outDir = do
  pursLog $ "Compiling Purescript project at " <> T.unpack (toTextIgnore workDir)
  compileAll workDir outDir

--------------------------------------------------------------------------------
jsNotFound :: T.Text -> String
jsNotFound js = printf [r|
You asked me to serve:

%s

But I wasn't able to find a suitable PureScript module to build.

If this is the first time you are running snaplet-purescript, have
a look inside snaplets/purs/Gruntfile.js.

You probably need to uncomment the 'main:' section to make it
point to your Main.purs, as well as adding any relevant module to
your 'modules:' section.
|] (T.unpack js)

--------------------------------------------------------------------------------
gruntTemplate :: String -> T.Text
gruntTemplate = T.pack . printf [r|
  module.exports = function(grunt) { "use strict"; grunt.initConfig({
    srcFiles: ["src/**/*.purs", "bower_components/**/src/**/*.purs"],
    psc: {
      options: {
          //main: "YourMainGoesHere",
          modules: [] //Add your modules here
      },
      all: {
        src: ["<%%=srcFiles%%>"],
        dest: "%s/app.js"
      }
    }
  });
  grunt.loadNpmTasks("grunt-purescript");
  grunt.registerTask("default", ["psc:all"]);
  };
|]

--------------------------------------------------------------------------------
envCfgTemplate :: Verbosity -> CompilationMode -> T.Text
envCfgTemplate ver cm = T.pack $ printf [r|
  # Choose one between 'Verbose' and 'Quiet'
  verbosity = "%s"
  # Choose one between 'CompileOnce' and 'CompileAlways'
  compilationMode = "%s"
|] (show ver) (show cm)

--------------------------------------------------------------------------------
mainTemplate :: T.Text
mainTemplate = T.pack [r|
  module Main where

  import Debug.Trace

  main = trace "Hello PS world!"
|]
