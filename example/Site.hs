{-# LANGUAGE OverloadedStrings #-}
module Site where

import           Snap.Snaplet
import           Data.ByteString

import           App
import           Snap.Snaplet.PureScript

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/purescript", with purs pursServe)]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    p <- nestSnaplet "purs" purs initPurs
    addRoutes routes
    return $ App p
