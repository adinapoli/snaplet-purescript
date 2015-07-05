{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.PureScript 
    ( initPurs
    , pursServe
    , module Internals
    ) where

import           Prelude hiding (FilePath)
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
  buildDir  <- liftIO (lookupDefault "js" config "buildDir")
  verbosity <- liftIO (lookupDefault Verbose config "verbosity")
  cm  <- getCompilationFlavour
  destDir    <- getDestDir
  srcDir     <- fromText <$> getSrcDir
  jsDir      <- fromText <$> getJsDir
  gruntfile  <- fromText <$> getGruntfile
  let envCfg = fromText $ destDir <> T.pack ("/" <> env <> ".cfg")
  -- If they do not exist, create the required directories
  shelly $ silently $ do
    srcDirExisted <- test_f srcDir
    mapM_ mkdir_p [jsDir, srcDir]
    gruntFileExists <- test_f gruntfile
    envCfgExists <- test_f envCfg
    unlessM localGruntInstalled $ chdir (fromText destDir) $ do
      liftIO $ putStrLn "Local grunt not found, installing it for you..."
      run_ "npm" ["install", "grunt"]
      run_ "npm" ["install", "grunt-purescript"]
    unless gruntFileExists $ writefile gruntfile (gruntTemplate buildDir)
    unless envCfgExists $ do
      touchfile envCfg
      writefile envCfg (envCfgTemplate verbosity cm)
    unless srcDirExisted $ do
      let mainFile = srcDir </> (fromText "Main.purs")
      touchfile mainFile
      writefile mainFile mainTemplate

  -- compile at least once, regardless of the CompilationMode
  compileAll srcDir

  return $ PureScript cm verbosity
  where
    description = "Automatic (re)compilation of PureScript projects"
    dataDir = liftM (++ "/resources") getDataDir

--------------------------------------------------------------------------------
pursLog :: String -> Handler b PureScript ()
pursLog l = do
  verb <- get >>= return . pursVerbosity
  unless (verb == Quiet) (liftIO $ putStrLn $ "snaplet-purescript: " <> l)

--------------------------------------------------------------------------------
localGruntInstalled :: Sh Bool
localGruntInstalled = errExit False $ do
  run_ "grunt" []
  eC <- lastExitCode
  return $ case eC of
    0 -> True
    _ -> False

--------------------------------------------------------------------------------
pursServe :: Handler b PureScript ()
pursServe = do
  modifyResponse . setContentType $ "text/javascript;charset=utf-8"
  get >>= compileWithMode . pursCompilationMode
  -- Now get the requested file and try to serve it
  -- This returns something like /purs/Hello/index.js
  (_, requestedJs) <- T.breakOn "/" . T.drop 1 . TE.decodeUtf8 . rqURI <$> getRequest
  case requestedJs of
    "" -> fail (jsNotFound requestedJs)
    _  -> do
      pursLog $ "Serving " <> T.unpack requestedJs
      jsDir <- getJsDir
      let fulljsPath = jsDir <> requestedJs
      (shelly $ silently $ readfile (fromText fulljsPath)) >>= writeText 

--------------------------------------------------------------------------------
compileAll :: MonadIO m => FilePath -> m ()
compileAll fp = liftIO $ shelly $ silently $ errExit False $ chdir fp $ do
  res <- run "grunt" []
  eC <- lastExitCode
  unless (eC == 0) $ error (show res)

--------------------------------------------------------------------------------
compileWithMode :: CompilationMode -> Handler b PureScript ()
compileWithMode CompileOnce = return ()
compileWithMode CompileAlways = do
  srcDir <- getSrcDir
  pursLog $ "Compiling from " <> T.unpack srcDir
  compileAll (fromText srcDir)

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
