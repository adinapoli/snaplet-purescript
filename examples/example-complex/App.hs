{-# LANGUAGE TemplateHaskell #-}

module App where

import           Control.Lens
import           Snap.Snaplet
import Snap.Snaplet.PureScript

data App = App {
      _purs :: Snaplet PureScript
    }

makeLenses ''App

type AppHandler = Handler App App
