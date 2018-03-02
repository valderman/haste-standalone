{-# LANGUAGE OverloadedStrings, CPP, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Create standalone Haste.App applications, which don't require a web server
--   or static Haste.App server/port configuration.
module Haste.App.Standalone (module Haste.App, runStandaloneApp) where
import Haste.App
#ifdef __HASTE__
import Control.Monad (void)
import Haste.Foreign
import System.IO.Unsafe

instance Node Server where
  endpoint proxy = unsafePerformIO $ do
    (h, p) <- getHostAndPort
    return $ remoteEndpoint h p proxy
#else
import Haste.App.Standalone.Server
#endif

-- | Run a Haste.App application in standalone mode: host/port configuration
--   is read from the command line, and a local web server is started to serve
--   the client program JS and any associated static files.
runStandaloneApp :: Client () -> IO ()
#ifdef __HASTE__
runStandaloneApp app = void $ setTimer (Once 0) $ do
  runApp [start (Proxy :: Proxy Server)] app

getHostAndPort :: IO (String, Int)
getHostAndPort =
  ffi "(function(){return [window['::hasteAppHost'], window['::hasteAppPort']];})"
#else
runStandaloneApp = runStandaloneServer
#endif
