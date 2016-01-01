{-# LANGUAGE OverloadedStrings #-}
module Haste.App.Standalone.Server (runStandaloneServer) where
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Embed
import Data.Embed.File
import qualified Data.Text as T
import Haste.App hiding (defaultConfig)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Directory hiding (findFile)
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import Haste.App.Standalone.Config

-- | Run application with settings obtained from the command line.
runStandaloneServer :: App Done -> IO ()
runStandaloneServer app = do
    (cfg, files) <- getConfig
    case runMode cfg of
      Server           -> runServer cfg app
      Embed js         -> embedFiles cfg js files
      PrintAndQuit msg -> putStr msg >> exitSuccess

-- | Embed the given JS and auxiliary files into this executable.
embedFiles :: Config -> FilePath -> [FilePath] -> IO ()
embedFiles cfg js aux = do
    when (jsMainExists && not (forceEmbed cfg)) $ do
      hPutStrLn stderr $ "This executable already contains a Haste.App " ++
                         "client JavaScript program; aborting.\n" ++
                         "To embed a new client program and auxiliary " ++
                         "files, use the `--force' option."
      exitFailure
    self <- getExecutablePath
    withTempFile (takeDirectory self) "" $ \tmp h -> do
      hClose h
      BS.readFile self >>= BS.writeFile tmp
      replaceBundle tmp $
        [ FileData jsFileNameFileName (BS.toStrict $ BS.pack jsFileName)
        , FilePath strip js
        ] ++ map (FilePath strip) aux
      copyPermissions self tmp
      renameFile tmp self
  where
    strip = maybe 0 id (stripDirs cfg)
    jsFileName = stripLeading strip js
    stripLeading 0 f = f
    stripLeading n f = stripLeading (n-1) (drop 1 (dropWhile (/= '/') f))

-- | Internal name of the file that contains the actual name of the app JS
--   file.
jsFileNameFileName :: FilePath
jsFileNameFileName = "\0__appJSFileName"

-- | Start the HTTP server serving up the application.
runServer :: Config -> App Done -> IO ()
runServer cfg app = do
  unless (jsMainExists) $ do
      hPutStrLn stderr $ "This executable does not seem to contain a " ++
                         "Haste.App client JavaScript program."
      hPutStrLn stderr $ "Please re-run it with the `--help' flag for " ++
                         "information on how to embed the\nclient JavaScript."
      exitFailure

  _ <- forkIO $ runApp (mkConfig (host cfg) (apiPort cfg)) app
  let jsMain = mkJSMain cfg
  run (httpPort cfg) $ \req respond -> do
    case T.unpack (T.concat (pathInfo req)) of
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

-- | Find an embedded file. Requests to @/@ and @/index.html@ always succeed.
findEmbeddedFile :: FilePath -> Maybe BS.ByteString
findEmbeddedFile "" =
  Just $ maybe defaultHTML BS.fromStrict (embeddedFile "index.html")
findEmbeddedFile "index.html" =
  Just $ maybe defaultHTML BS.fromStrict (embeddedFile "index.html")
findEmbeddedFile path =
  BS.fromStrict <$> embeddedFile path

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

-- | Default HTML skeleton.
defaultHTML :: BS.ByteString
defaultHTML = BS.concat
    [ "<!DOCTYPE HTML><html><head>"
    , "<title>Standalone Haste.App application</title>"
    , "<meta charset=\"UTF-8\">"
    , "<script type=\"text/javascript\" src=\"", js, "\"></script>"
    , "</head><body></body></html>"
    ]
  where
    js = BS.pack jsMainFileName

-- | Embedded filename of the main JS program.
jsMainFileName :: FilePath
jsMainFileName = BS.unpack $ BS.fromStrict (embeddedFile' jsFileNameFileName)

-- | Does the JS main program exist, or do we need to embed it?
jsMainExists :: Bool
jsMainExists
  | Just _ <- embeddedFile jsMainFileName = True
  | otherwise                             = False

-- | The client application JS.
mkJSMain :: Config -> BS.ByteString
mkJSMain cfg = BS.concat
  [ "window['::hasteAppHost']='", BS.pack (host cfg), "';"
  , "window['::hasteAppPort']=", BS.pack (show $ apiPort cfg), ";"
  , BS.fromStrict (embeddedFile' jsMainFileName)
  ]
