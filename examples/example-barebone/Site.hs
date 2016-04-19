{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Site where

import           Data.ByteString
import           Snap.Snaplet
import           Text.RawString.QQ

import           App
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet.PureScript

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/", ifTop renderIndex)
         , ("/purescript", with purs pursServe)
         ]

renderIndex :: Handler b v ()
renderIndex = writeText . T.pack $ [r|
<html>
  <head>
    <script type='text/javascript' src="purescript/app.js"></script>
    <script type='text/javascript' src="https://code.jquery.com/jquery-1.12.3.min.js"></script>
  </head>
  <body>
    <div id='helloPS'>It worked! Check your JS console.</div>
  </body>
  <script>
   $(document).on("ready", function() { PS.Main.main(); });
  </script>
</html>
|]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    p <- nestSnaplet "purs" purs initPurs
    addRoutes routes
    return $ App p
