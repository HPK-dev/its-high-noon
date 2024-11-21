{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Database (
    DataFields (..),
    QueryFields (..),
    DBConfig (..),
    connectDB,
    makeQuery,
    randomQuestion,
    QueryError (..),
) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Default (Default (..))
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Database.PostgreSQL.Simple (
    ConnectInfo (..),
    Connection,
    connect,
    defaultConnectInfo,
    query,
 )
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToField (Action, ToField (toField))
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Database.PostgreSQL.Simple.Types (Query (..))
import GHC.Generics (Generic)
import System.Environment (getEnv)

-- | Database configuration
data DBConfig = DBConfig
    { dbHost :: String
    , dbName :: String
    , dbUser :: String
    , dbPassword :: String
    }
    deriving (Show, Generic)

-- | Custom errors
data QueryError
    = NoResults
    | InvalidQuery Text
    deriving (Show, Exception)

-- | Main data type for Questions table
data DataFields
    = DataFields
    { dataId :: Integer
    , dataSubject :: Text
    , dataDescription :: Text
    , dataOpts :: Text
    , dataAns :: Text
    , dataExplanation :: Maybe Text
    , dataDetails :: Maybe Text
    }
    deriving (Show, Generic, FromRow)

-- | Query builder type
data QueryFields
    = QueryFields
    { queryId :: Maybe Integer
    , querySubject :: Maybe Text
    , queryDescription :: Maybe Text
    , queryOpts :: Maybe Text
    , queryAns :: Maybe Text
    , queryExplanation :: Maybe Text
    , queryDetails :: Maybe Text
    }
    deriving (Show, Generic, ToRow)

instance Default QueryFields where
    def =
        QueryFields
            { queryId = Nothing
            , querySubject = Nothing
            , queryDescription = Nothing
            , queryOpts = Nothing
            , queryAns = Nothing
            , queryExplanation = Nothing
            , queryDetails = Nothing
            }

-- | Load database configuration from environment
loadDBConfig :: IO DBConfig
loadDBConfig = do
    loadFile defaultConfig
    DBConfig
        <$> getEnv "DB_HOST"
        <*> getEnv "DB_NAME"
        <*> getEnv "DB_USER"
        <*> getEnv "DB_PASS"

-- | Create database connection
connectDB :: IO Connection
connectDB = do
    DBConfig {..} <- loadDBConfig
    connect
        defaultConnectInfo
            { connectHost = dbHost
            , connectDatabase = dbName
            , connectUser = dbUser
            , connectPassword = dbPassword
            }

-- | Build WHERE clause for SQL query
buildWhereClause :: [(String, Action)] -> (Query, [Action])
buildWhereClause [] = ("", [])
buildWhereClause conditions =
    let clause =
            " WHERE "
                <> fromString (intercalate " AND " [c <> " ?" | (c, _) <- conditions])
        params = map snd conditions
     in (clause, params)

-- | Create and execute database query
makeQuery :: Connection -> QueryFields -> Maybe String -> IO [DataFields]
makeQuery conn QueryFields {..} additional = do
    let conditions =
            catMaybes
                [ ("id",) . toField <$> queryId
                , ("subject",) . toField <$> querySubject
                , ("description",) . toField <$> queryDescription
                , ("opts",) . toField <$> queryOpts
                , ("ans",) . toField <$> queryAns
                , ("explanation",) . toField <$> queryExplanation
                , ("details",) . toField <$> queryDetails
                ]
    let (whereClause, params) = buildWhereClause conditions
        additionalClause = maybe "" fromString additional
        finalQuery = "SELECT * FROM Questions" <> whereClause <> additionalClause

    query conn finalQuery params

-- | Get a random question
randomQuestion :: Connection -> QueryFields -> Integer -> IO [DataFields]
randomQuestion conn qf amt = do
    when (amt <= 0) $ throwIO $ InvalidQuery "Amount must be positive"
    result <- makeQuery conn qf (Just $ " ORDER BY RANDOM() LIMIT " <> show amt)
    case result of
        [] -> throwIO NoResults
        xs -> pure xs