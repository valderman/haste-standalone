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
import Network.Info
import Network.Wai
import Network.Wai.Handler.Warp
import System.Console.GetOpt
import System.Directory hiding (findFile)
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.IO.Unsafe

-- | What should we do when we start?
data RunMode
  = Server
  | PrintAndQuit String
  | Embed FilePath
    deriving Show

-- | Configuration for a standalone application.
data Config = Config
  { apiPort      :: Int
  , httpPort     :: Int
  , host         :: String
  , runMode      :: RunMode
  , dataDir      :: Maybe FilePath
  , stripDirs    :: Maybe Int
  , forceEmbed   :: Bool
  } deriving Show

defaultConfig :: Config
defaultConfig = Config
  { apiPort      = 24601
  , httpPort     = 8080
  , host         = unsafePerformIO autodetectHost
  , runMode      = Server
  , dataDir      = Nothing
  , stripDirs    = Nothing
  , forceEmbed   = False
  }

readWithDefault :: Read a => a -> String -> a
readWithDefault def s
  | [(x, "")] <- reads s = x
  | otherwise            = def

optspec :: [OptDescr (Config -> Config)]
optspec =
  [ Option "a" ["api-port"]
    (ReqArg (\p c -> c {apiPort = readWithDefault (apiPort c) p}) "PORT") $
    "Port to use for internal app communication.\n" ++
    "Default: 24601"
  , Option "h" ["host"]
    (ReqArg (\h c -> c {host = h}) "HOST") $
    "Host from which this application is served.\n" ++
    "Default: autodetect"
  , Option "p" ["http-port"]
    (ReqArg (\p c -> c {httpPort = readWithDefault (httpPort c) p}) "PORT") $
    "Port on which users can access the application through their web " ++
    "browser.\n" ++
    "Default: 8080"
  , Option "d" ["data-directory"]
    (ReqArg (\d c -> c {dataDir = Just d}) "DIR") $
    "Directory from which to serve static files.\n" ++
    "Default: none"
  , Option "e" ["embed"]
    (ReqArg (\js c -> c {runMode = Embed js}) "JS") $
    "Embed JS as the Haste.App client JavaScript file for this " ++
    "application.\n" ++
    "Any non-option arguments are interpreted as file " ++
    "names to embed alongside JS."
  , Option "f" ["force"]
    (NoArg (\c -> c {forceEmbed = True})) $
    "Overwrite any JavaScript and static files already embedded in this " ++
    "executable.\n"
  , Option "s" ["strip-dirs"]
    (ReqArg (\n c -> c {stripDirs = Just (readWithDefault 0 n)}) "N") $
    "Strip the N first leading directories of file names provided with " ++
    "`--embed'.\n" ++
    "Default: 0."
  , Option "?" ["help"]
    (NoArg (\c -> c {runMode = PrintAndQuit help})) $
    "Print this help message and exit."
  ]

helpHeader :: String
helpHeader = concat
  [ "This web application was built using haste-standalone."
  , " It accepts the following options:"
  ]

-- | Help message for @--help@.
help :: String
help = usageInfo helpHeader optspec

-- | Read configuration from command line and parse it.
getConfig :: IO (Config, [FilePath])
getConfig = do
  args <- getArgs
  case getOpt Permute optspec args of
    (opts, fs, []) -> return ((foldr (flip (.)) (id) opts) defaultConfig, fs)
    (_, _, errs)   -> mapM_ (hPutStr stderr) errs >> exitFailure

-- | Attempt to autodetect the host we're currently running on.
--   Defaults to @localhost@ if no interface could be detected.
autodetectHost :: IO String
autodetectHost = do
    ifs <- map (show . ipv4) <$> getNetworkInterfaces
    return $ head $ filter isValidIpv4 ifs ++ ["localhost"]
  where
    isValidIpv4 ('0':_)         = False
    isValidIpv4 ('1':'2':'7':_) = False
    isValidIpv4 addr            = not $ and $ zipWith (==) addr "169.254"

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
findFile cfg path = do
  mabspath <- case dataDir cfg of
    Just d -> do
      -- Check that path exists
      mp <- catch (Just <$> (makeAbsolute (d </> path) >>= canonicalizePath))
                  (\(SomeException _) -> pure Nothing)

      -- Check that path is inside data directory
      datadir <- makeAbsolute d >>= canonicalizePath
      let index = fmap (</> "index.html") mp
      case mp of
        Just path'
          | datadir == path'                 -> pure index
          | and $ zipWith (==) datadir path' -> pure mp
        _                                    -> pure Nothing
    _ -> do
      pure Nothing

  -- Files in data directory override embedded files of the same name
  case (path, mabspath, embeddedFile path) of
    (_, (Just abspath), _) -> pure $ responseFile ok200 [] abspath Nothing
    (_, _, Just x)         -> pure $ responseLBS ok200 [] (BS.fromStrict x)
    ("", _, _)             -> pure $ responseLBS ok200 [] defaultHTML
    ("index.html", _, _)   -> pure $ responseLBS ok200 [] defaultHTML
    _                      -> pure $ responseLBS status404 [] "404"

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
