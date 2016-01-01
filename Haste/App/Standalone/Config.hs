-- | Configuration and command line handling.
module Haste.App.Standalone.Config where
import Network.Info
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe

-- | What should we do when we start?
data RunMode
  = Server
  | PrintAndQuit String
  | Embed FilePath
  | ListEmbedded
    deriving Show

-- | Configuration for a standalone application.
data Config = Config
  { apiPort      :: Int
  , httpPort     :: Int
  , host         :: String
  , runMode      :: RunMode
  , dataDir      :: Maybe FilePath
  , dataDirFirst :: Bool
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
  , dataDirFirst = False
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
    "If an embedded file exists in DIR as well, the embedded version will" ++
    "shadow the one in DIR.\n" ++
    "This means that `/' and `/index.html' will " ++
    "always be served as their embedded versions.\n" ++
    "Use `--override-embedded' to allow files in DIR to shadow embedded " ++
    "ones.\n" ++
    "Default: none"
    
  , Option "o" ["override-embedded"]
    (NoArg (\c -> c {dataDirFirst = True})) $
    "Allow files from the data directory to shadow embedded files.\n" ++
    "The Haste.App client JavaScript program can never be shadowed, however."
    
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

  , Option "l" ["list-files"]
    (NoArg (\c -> c {runMode = ListEmbedded})) $
    "List all files embedded in this executable. The app JavaScript file " ++
    "will be prefixed with an asterisk."

  , Option "?" ["help"]
    (NoArg (\c -> c {runMode = PrintAndQuit help})) $
    "Print this help message."
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
