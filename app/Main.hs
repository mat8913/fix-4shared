-- Copyright (C) 2017  Matthew Harm Bekkema
--
-- This file is part of fix-4shared.
--
-- fix-4shared is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- fix-4shared is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

import           Data.Function          (fix)
import           Data.Foldable          (for_)
import           Control.Monad          (unless)
import           Control.Monad.Reader
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Control.Exception  (throwIO)
import           System.Environment (getArgs)
import           System.IO          (hPutStr, hPutStrLn, hFlush, stderr)

import           Data.Conduit
import qualified Data.Conduit.List as C

import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)

import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as L
import           Data.ByteString.Builder (toLazyByteString, integerDec)
import qualified Data.ByteString.Base16  as Base16

import           Data.Yaml     (decodeFileEither, encode)
import           Data.Aeson.TH (deriveJSON, defaultOptions)
import           Data.Aeson    (Value, FromJSON (parseJSON), (.:), withObject,
                                eitherDecode')

import qualified Crypto.Hash.MD5 as MD5

import           Network.HTTP.Client.TLS   (getGlobalManager)
import           Network.HTTP.Client
    ( Request (secure, host, port, checkResponse, method, path, queryString)
    , defaultRequest, urlEncodedBody, Response, responseBody, responseStatus, Manager
    , brReadSome, HttpException (HttpExceptionRequest)
    , HttpExceptionContent (StatusCodeException)
    )
import qualified Network.HTTP.Client       as HTTP
import           Network.HTTP.Types.Status (statusCode)
import           Network.HTTP.Types.URI    (renderQuery)
import qualified Network.HTTP.Types.URI    as HTTP

import           Web.Authenticate.OAuth
    ( OAuth (oauthConsumerKey, oauthConsumerSecret, oauthServerName
    , oauthRequestUri, oauthAccessTokenUri, oauthAuthorizeUri, oauthVersion
    , oauthCallback), OAuthVersion (OAuth10), newOAuth, Credential, signOAuth
    , getTemporaryCredential, authorizeUrl, getAccessToken
    )


data Config = Config { consumerKey    :: Text
                     , consumerSecret :: Text
                     }
$(deriveJSON defaultOptions ''Config)

data Path = Path { pathId   :: Text
                 , pathName :: Text
                 }
  deriving Show

newtype Files = Files [Path]
newtype Folders = Folders [Path]

instance FromJSON Path where
    parseJSON = withObject "Path" $ \v -> Path
        <$> v .: "id"
        <*> v .: "name"

instance FromJSON Files where
    parseJSON = withObject "Files" $ \v -> Files
        <$> v .: "files"

instance FromJSON Folders where
    parseJSON = withObject "Folders" $ \v -> Folders
        <$> v .: "folders"

data IConfig = IConfig { manager :: Manager
                       , oauth   :: OAuth
                       , creds   :: Credential
                       }

main :: IO ()
main = do
    args <- getArgs
    case args of
        [rootDir] -> app $ T.pack rootDir
        []        -> showUserInfo
        _         -> fail "Invalid usage."

app :: Text -> IO ()
app rootDir = do
    config <- getConfig
    flip runReaderT config $ do
        tmpFolder <- findFolder rootDir "tmp"
        runConduit $ files tmpFolder
                  .| awaitForever (\x -> do
                                  dstId <- findCorrectDirId rootDir (pathName x)
                                  move (pathId x) dstId)

showUserInfo :: IO ()
showUserInfo = do
    config <- getConfig
    flip runReaderT config $ do
        x' <- user
        case eitherDecode' x' of
            Left e  -> fail e
            Right x -> liftIO $ BS.putStr $ encode (x :: Value)

getConfig :: IO IConfig
getConfig = do
    Config {..} <- either throwIO pure =<< decodeFileEither "4shared.yaml"
    _manager <- getGlobalManager
    let _oauth = newOAuth { oauthConsumerKey = encodeUtf8 consumerKey
                          , oauthConsumerSecret = encodeUtf8 consumerSecret
                          , oauthServerName = "4shared"
                          , oauthRequestUri = "https://api.4shared.com/v1_2/oauth/initiate"
                          , oauthAccessTokenUri = "https://api.4shared.com/v1_2/oauth/token"
                          , oauthAuthorizeUri = "https://api.4shared.com/v1_2/oauth/authorize"
                          , oauthVersion = OAuth10
                          , oauthCallback = Just "http://localhost"
                          }
    tempCreds <- getTemporaryCredential _oauth _manager
    hPutStrLn stderr $ "Authorize: " ++ authorizeUrl _oauth tempCreds
    hPutStr stderr "Enter to continue..."
    hFlush stderr
    _ <- getLine
    _creds <- getAccessToken _oauth tempCreds _manager
    pure $ IConfig { manager = _manager
                   , oauth = _oauth
                   , creds = _creds
                   }

findFolder :: (MonadIO m, MonadReader IConfig m)
           => Text   -- ^ Directory ID to look in
           -> Text   -- ^ Directory name to find
           -> m Text -- ^ Directory ID found
findFolder dirId search = do
    mx <- runConduit $ folders dirId
                    .| C.filter ((search==) . pathName)
                    .| await
    case mx of
        Nothing -> fail $ "Couldn't find " ++ T.unpack search ++ " directory."
        Just x  -> pure $ pathId x

user :: (MonadIO m, MonadReader IConfig m) => m L.ByteString
user = fmap responseBody $ httpLbs $ baseRequest { method = "GET"
                                                 , path = "/v1_2/user"
                                                 }

folders :: (MonadIO m, MonadReader IConfig m) => Text -> ConduitM i Path m ()
folders dirId = folders' dirId .| parseFolders

files :: (MonadIO m, MonadReader IConfig m) => Text -> ConduitM i Path m ()
files dirId = files' dirId .| parseFiles

parseFiles :: Monad m => ConduitM L.ByteString Path m ()
parseFiles = fix $ \r -> do
    mx <- await
    case mx of
        Nothing -> pure ()
        Just x  -> case eitherDecode' x of
            Left e           -> fail e
            Right (Files []) -> pure ()
            Right (Files xs) -> C.sourceList xs >> r

parseFolders :: Monad m => ConduitM L.ByteString Path m ()
parseFolders = fix $ \r -> do
    mx <- await
    case mx of
        Nothing -> pure ()
        Just x  -> case eitherDecode' x of
            Left e           -> fail e
            Right (Folders []) -> pure ()
            Right (Folders xs) -> C.sourceList xs >> r

files' :: (MonadIO m, MonadReader IConfig m) => Text -> ConduitM i L.ByteString m ()
files' dirId = for_ [0..] $ \p -> do
    let req = baseRequest { method = "GET"
                          , path = encodePathSegments ["v1_2", "folders", dirId, "files"]
                          , queryString = pagination p
                          }
    res <- httpLbs req
    yield $ responseBody res

folders' :: (MonadIO m, MonadReader IConfig m) => Text -> ConduitM i L.ByteString m ()
folders' dirId = for_ [0..] $ \p -> do
    let req = baseRequest { method = "GET"
                          , path = encodePathSegments ["v1_2", "folders", dirId, "children"]
                          , queryString = pagination p
                          }
    res <- httpLbs req
    yield $ responseBody res

move :: (MonadIO m, MonadReader IConfig m) => Text -> Text -> m ()
move srcId dstId = do
    let req =  baseRequest { method = "POST"
                           , path = encodePathSegments ["v1_2", "files", srcId, "move"]
                           }
        req' = urlEncodedBody [("folderId", encodeUtf8 dstId)] req
    void $ httpNoBody req'

findCorrectDirId :: (MonadIO m, MonadReader IConfig m) => Text -> Text -> m Text
findCorrectDirId baseDirId key = do
    let (d1, d2) = getCorrectDir key
    d1Id <- findFolder baseDirId d1
    d2Id <- findFolder d1Id d2
    findFolder d2Id key


baseRequest :: Request
baseRequest = defaultRequest { secure = True
                             , host = "api.4shared.com"
                             , port = 443
                             , checkResponse = checkStatus
                             }

checkStatus :: Request -> Response HTTP.BodyReader -> IO ()
checkStatus req res = unless (200 <= sci && sci < 300) $ do
    chunk <- brReadSome (responseBody res) 1024
    let res' = fmap (const ()) res
    throwIO $ HttpExceptionRequest req $
        StatusCodeException res' (L.toStrict chunk)
  where
    sci = statusCode $ responseStatus res

pagination :: Integer -> ByteString
pagination p = renderQuery True
    [ ("limit", Just "100")
    , ("offset", Just $ L.toStrict $ toLazyByteString $ integerDec $ 100 * p)
    ]

encodePathSegments :: [Text] -> ByteString
encodePathSegments = L.toStrict . toLazyByteString . HTTP.encodePathSegments

httpLbs :: (MonadIO m, MonadReader IConfig m) => Request -> m (Response L.ByteString)
httpLbs req = do
    IConfig {..} <- ask
    liftIO . flip HTTP.httpLbs manager =<< signOAuth oauth creds req

httpNoBody :: (MonadIO m, MonadReader IConfig m) => Request -> m (Response ())
httpNoBody req = do
    IConfig {..} <- ask
    liftIO . flip HTTP.httpNoBody manager =<< signOAuth oauth creds req

getCorrectDir :: Text -> (Text, Text)
getCorrectDir key = (decodeUtf8 p1, decodeUtf8 p2)
  where
    hash = Base16.encode $ MD5.hash $ encodeUtf8 key
    (p1, hash') = BS.splitAt 3 hash
    p2 = BS.take 3 hash'
