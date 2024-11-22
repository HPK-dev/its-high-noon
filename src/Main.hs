{-# LANGUAGE OverloadedStrings #-}

import Configuration.Dotenv (defaultConfig, loadFile)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database (DBConfig (..), connectDB)
import System.Environment (getEnv)
import System.Log.FastLogger (
    LogType' (LogStdout),
    defaultBufSize,
    newTimedFastLogger,
 )
import System.Log.FastLogger.Date (newTimeCache)
import WebhookServer (RuntimeData (..), startServer)

-- | Load database configuration from environment
loadDBConfig :: IO DBConfig
loadDBConfig = do
    DBConfig
        <$> getEnv "DB_HOST"
        <*> getEnv "DB_NAME"
        <*> getEnv "DB_USER"
        <*> getEnv "DB_PASS"

main :: IO ()
main = do
    loadFile defaultConfig
    -- let database = connectDB =<< loadDBConfig

    secret <- encodeUtf8 . T.pack <$> getEnv "LINEBOT_CHANNEL_SECRET"
    token <- encodeUtf8 . T.pack <$> getEnv "LINEBOT_CHANNEL_ACCESS_TOKEN"

    timeCache <- newTimeCache "%Y-%m-%d %H:%M:%S"
    (logger', cleanup) <- newTimedFastLogger timeCache (LogStdout defaultBufSize)

    let config = RuntimeData {channelSecret = secret, accessToken = token, logger = logger'}

    let port = 8080
    startServer port config