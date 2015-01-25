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
    unless gruntFileExists $ writefile gruntfile (gruntTemplate buildDir)
    unless envCfgExists $ do
      touchfile envCfg
      writefile envCfg (envCfgTemplate cm)
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
pursServe :: Handler b PureScript ()
pursServe = do
  modifyResponse . setContentType $ "text/javascript;charset=utf-8"
  get >>= compileWithMode . pursCompilationMode
  -- Now get the requested file and try to serve it
  -- This returns something like /purs/Hello/index.js
  (_, requestedJs) <- T.breakOn "/" . T.drop 1 . TE.decodeUtf8 . rqURI <$> getRequest
  case requestedJs of
    "" -> fail "The path you asked me to serve doesn't point anywhere!"
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
gruntTemplate :: String -> T.Text
gruntTemplate = T.pack . printf [r|
  module.exports = function(grunt) { "use strict"; grunt.initConfig({
    srcFiles: ["src/**/*.purs", "bower_components/**/src/**/*.purs"],
    pscMake: {
      options: {
          //main: "YourMainGoesHere",
          modules: [] //Your modules list goes here
      },
      lib: {
        src: ["<%%=srcFiles%%>"],
        dest: "%s"
      }
    }
  });
  grunt.loadNpmTasks("grunt-purescript");
  grunt.registerTask("default", ["pscMake:lib"]);
  };
|]

--------------------------------------------------------------------------------
envCfgTemplate :: CompilationMode -> T.Text
envCfgTemplate cm = T.pack $ printf [r|
  # Choose one between 'CompileOnce' and 'CompileAlways'
  compilationMode = "%s"
|] (show cm)

--------------------------------------------------------------------------------
mainTemplate :: T.Text
mainTemplate = T.pack [r|
  module Main where

  import Debug.Trace

  main = trace "Hello PS world!"
|]
