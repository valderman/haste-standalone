{-# LANGUAGE OverloadedStrings #-}
-- | Creating and reading file embeddings.
module Haste.App.Standalone.Embed
  ( embedFiles , findEmbeddedFile, embeddedFiles
  , mkJSMain, jsMainExists , jsMainFileName
  ) where
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Embed
import Data.Embed.File
import System.Environment (getExecutablePath)
import System.Exit
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp
import Haste.App.Standalone.Config

-- | List all files embedded in this executable.
embeddedFiles :: [FilePath]
embeddedFiles =
    [markAppFile f | f <- listBundleFiles myBundle, f /= jsFileNameFileName]
  where
    markAppFile f
      | f == jsMainFileName = '*':f
      | otherwise           = f

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

-- | Find an embedded file. Requests to @/@ and @/index.html@ always succeed.
findEmbeddedFile :: FilePath -> Maybe BS.ByteString
findEmbeddedFile "" =
  Just $ maybe defaultHTML BS.fromStrict (embeddedFile "index.html")
findEmbeddedFile "index.html" =
  Just $ maybe defaultHTML BS.fromStrict (embeddedFile "index.html")
findEmbeddedFile path =
  BS.fromStrict <$> embeddedFile path

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
  [ BS.fromStrict (embeddedFile' jsMainFileName)
  , "window['::hasteAppHost']='", BS.pack (host cfg), "';"
  , "window['::hasteAppPort']=", BS.pack (show $ apiPort cfg), ";"
  ]

-- | Internal name of the file that contains the actual name of the app JS
--   file.
jsFileNameFileName :: FilePath
jsFileNameFileName = "\0__appJSFileName"

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
