{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Haste.App.Standalone.Server (runStandaloneServer) where
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Text as T
import Haste.App
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Directory hiding (findFile)
import System.Exit
import System.FilePath
import System.IO
import Haste.App.Standalone.Config
import Haste.App.Standalone.Embed
import System.IO.Unsafe
import Data.IORef

{-# NOINLINE endpointRef #-}
endpointRef :: IORef Endpoint
endpointRef = unsafePerformIO $ newIORef (error "endpoint not initialized")

instance Node Server where
  endpoint _ = unsafePerformIO $ readIORef endpointRef

-- | Run application with settings obtained from the command line.
runStandaloneServer :: [NodeConfig] -> Client () -> IO ()
runStandaloneServer nodes app = do
  (cfg, files) <- getConfig
  case runMode cfg of
    Server           -> runServer nodes cfg app
    Embed js         -> embedFiles cfg js files
    ListEmbedded     -> mapM_ putStrLn embeddedFiles >> exitSuccess
    PrintAndQuit msg -> putStr msg >> exitSuccess

-- | Start the HTTP server serving up the application.
runServer :: [NodeConfig] -> Config -> Client () -> IO ()
runServer nodes cfg app = do
  -- Check that we at least have a main JS file before running
  unless (jsMainExists) $ do
    hPutStrLn stderr $ "This executable does not seem to contain a " ++
                       "Haste.App client JavaScript program."
    hPutStrLn stderr $ "Please re-run it with the `--help' flag for " ++
                       "information on how to embed the\nclient JavaScript."
    exitFailure

  -- Set up working directory and print diagnostics
  setCurrentDirectory (workDir cfg)
  let hoststr = "http://" ++ host cfg ++ ":" ++ show (httpPort cfg)
  putStrLn $ "Application started on " ++ hoststr

  writeIORef endpointRef $ remoteEndpoint (host cfg)
                                          (apiPort cfg)
                                          (Proxy :: Proxy Server)
  _ <- forkIO $ runApp (start (Proxy :: Proxy Server):nodes) app
  let jsMain = mkJSMain cfg
  run (httpPort cfg) $ \req respond -> do
    case T.unpack (T.intercalate "/" (pathInfo req)) of
      -- Haste.App JS file is always served embedded
      path | path == jsMainFileName -> respond $ responseLBS ok200 [] jsMain
           | otherwise              -> findFile cfg path >>= respond

-- | Find and return a file corresponding to the given path.
--   TODO: cache files to avoid having to call 'embeddedFile' all the time.
findFile :: Config -> FilePath -> IO Response
findFile cfg path
  | dataDirFirst cfg = do
    mf <- findDataFile (dataDir cfg) path
    case (mf, findEmbeddedFile path) of
      (Just f, _) -> pure $ responseFile ok200 [] f Nothing
      (_, Just f) -> pure $ responseLBS ok200 [] f
      _           -> pure $ responseLBS status404 [] "404"
  | otherwise = do
    case findEmbeddedFile path of
      Just f -> pure $ responseLBS ok200 [] f
      _      -> do
        mf <- findDataFile (dataDir cfg) path
        case mf of
          Just f -> pure $ responseFile ok200 [] f Nothing
          _      -> pure $ responseLBS status404 [] "404"

-- | Find a file in the specified data directory.
findDataFile :: Maybe FilePath -> FilePath -> IO (Maybe FilePath)
findDataFile (Just datadir) path = do
  -- Check that path exists
  mp <- catch (Just <$> (makeAbsolute (datadir </> path) >>= canonicalizePath))
              (\(SomeException _) -> pure Nothing)

  -- Check that path is inside data directory
  datadir' <- makeAbsolute datadir >>= canonicalizePath
  let index = fmap (</> "index.html") mp
  case mp of
    Just path'
      | datadir' == path'                 -> pure index
      | and $ zipWith (==) datadir' path' -> pure mp
    _                                     -> pure Nothing
findDataFile _ _ = do
  pure Nothing
