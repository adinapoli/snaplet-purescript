{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.PureScript.Hooks (
  getHooks
  , Hook
  , Hooks(..)
  ) where

import           Data.Configurator
import           Data.Configurator.Types (Config)
import qualified Data.Text as T
import           Shelly

--------------------------------------------------------------------------------
type Hook = Sh ()

--------------------------------------------------------------------------------
data Hooks = Hooks {
    preInitHook     :: Hook
  , postInitHook    :: Hook
  , preBuildHook    :: Hook
  , postBuildHook   :: Hook
  , preBundleHook   :: Hook
  , postBundleHook  :: Hook
  }

instance Show Hooks where
  show _ = "<<hooks>>"

--------------------------------------------------------------------------------
noOpHook :: Hook
noOpHook = return ()

--------------------------------------------------------------------------------
mkHook :: T.Text -> Hook
mkHook "" = noOpHook
mkHook t  = case T.words t of
  [] -> noOpHook
  (x:args) -> run_ (fromText x) args

--------------------------------------------------------------------------------
getHooks :: Config -> IO Hooks
getHooks cfg =
  Hooks <$> (mkHook <$> lookupDefault "" cfg "hooks.preInit")
        <*> (mkHook <$> lookupDefault "" cfg "hooks.postInit")
        <*> (mkHook <$> lookupDefault "" cfg "hooks.preBuild")
        <*> (mkHook <$> lookupDefault "" cfg "hooks.postBuild")
        <*> (mkHook <$> lookupDefault "" cfg "hooks.preBundle")
        <*> (mkHook <$> lookupDefault "" cfg "hooks.postBundle")
