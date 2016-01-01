{-# LANGUAGE OverloadedStrings, CPP #-}
-- | Create standalone Haste.App applications, which don't require a web server
--   or static Haste.App server/port configuration.
module Haste.App.Standalone (module Haste.App, runStandaloneApp) where
import Haste.App
#ifdef __HASTE__
import Control.Monad (void)
import Haste
import Haste.Foreign
#else
import Haste.App.Standalone.Server
#endif

-- | Run a Haste.App application in standalone mode: host/port configuration
--   is read from the command line, and a local web server is started to serve
--   the client program JS and any associated static files.
runStandaloneApp :: App Done -> IO ()
#ifdef __HASTE__
runStandaloneApp app = void $ setTimer (Once 0) $ do
  (host, port) <- getHostAndPort
  runApp (mkConfig host port) app

getHostAndPort :: IO (String, Int)
getHostAndPort =
  ffi "(function(){return [window['::hasteAppHost'], window['::hasteAppPort']];})"
#else
runStandaloneApp = runStandaloneServer
#endif
