{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module WebhookServer where

import Control.Exception (SomeException (..), catch)
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (getCurrentTime)
import GHC.Generics
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import System.Log.FastLogger

-- New Types for Request Body
data WebhookEvent = WebhookEvent
    {
        type ::Text,
    }
    -- Add webhook event fields as needed

    deriving (Generic, Show)

data WebhookRequest = WebhookRequest
    { destination :: Text
    , events :: [WebhookEvent]
    }
    deriving (Generic, Show)

instance FromJSON WebhookEvent

instance ToJSON WebhookEvent

instance FromJSON WebhookRequest

instance ToJSON WebhookRequest

-- Runtime Data and Config
data RuntimeData = RuntimeData
    { channelSecret :: ByteString
    , accessToken :: ByteString
    , logger :: TimedFastLogger
    }

type WebhookAPI =
    Post '[JSON] Text
        :<|> "callback"
            :> Servant.Header "X-Line-Signature" Text
            :> ReqBody '[JSON] WebhookRequest
            :> Post '[JSON] Text

-- Middleware Stack
loggerMiddleware :: RuntimeData -> Middleware
loggerMiddleware config app req respond = do
    timestamp <- getCurrentTime
    let logMsg =
            toLogStr $
                show timestamp
                    <> " "
                    <> show (requestMethod req)
                    <> " "
                    <> show (rawPathInfo req)
                    <> "\n"
    liftIO $ logger config (const logMsg)
    app req respond

corsMiddleware :: Middleware
corsMiddleware app req respond = app req $ \response ->
    respond $ mapResponseHeaders appendCorsHeaders response
    where
        appendCorsHeaders headers =
            headers
                ++ [ ("Access-Control-Allow-Origin", "*")
                   , ("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
                   , ("Access-Control-Allow-Headers", "X-Line-Signature, Content-Type")
                   ]

errorMiddleware :: RuntimeData -> Middleware
errorMiddleware config app req respond =
    app req respond `catch` \(e :: SomeException) -> do
        let errMsg = "Error: " <> show e
        liftIO $ logger config (\_ -> toLogStr $ errMsg <> "\n")
        respond $
            responseLBS
                status500
                [("Content-Type", "text/plain")]
                (LBS.fromStrict $ encodeUtf8 $ "Error: " <> T.pack (show e))

-- Core Server Logic
webhookServer :: RuntimeData -> Server WebhookAPI
webhookServer config = rootHandler :<|> webhookHandler
    where
        rootHandler = return "OK"
        webhookHandler (Just sig) request = do
            let body = encode request
            liftIO $
                logMessage config $
                    "Webhook request: " <> decodeUtf8 (LBS.toStrict body)
            let signature = encodeUtf8 sig
            if verifySignature (channelSecret config) (LBS.toStrict body) signature
                then do
                    handleWebhook config request signature
                    return "OK"
                else throwError err401 {errBody = "Invalid signature"}
        webhookHandler Nothing _ = throwError err401 {errBody = "Missing signature header"}

handleWebhook :: RuntimeData -> WebhookRequest -> ByteString -> Handler ()
handleWebhook config request sig = do
    liftIO $ logMessage config "Processing webhook..."
    liftIO $ logMessage config $ "Destination: " <> destination request
    liftIO $
        logMessage config $
            "Number of events: " <> decodeUtf8 (C8.pack (show (length (events request))))
    pure ()

-- Rest of the code remains the same...
verifySignature :: ByteString -> ByteString -> ByteString -> Bool
verifySignature secret body signature =
    C8.strip (B64.encode $ SHA256.hmac secret body) == C8.strip signature

logMessage :: RuntimeData -> Text -> IO ()
logMessage config msg =
    logger config (\time -> toLogStr (show time) <> " " <> toLogStr msg <> "\n")

app :: RuntimeData -> Application
app config =
    loggerMiddleware config
        . corsMiddleware
        . errorMiddleware config
        $ serve (Proxy :: Proxy WebhookAPI) (webhookServer config)

startServer :: Int -> RuntimeData -> IO ()
startServer port config = do
    putStrLn $ "Starting server on port " ++ show port
    run port (app config)