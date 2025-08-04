module Masna3.Server.Config where

import Auth.Biscuit
import Auth.Biscuit.Utils
import Control.Monad
import Data.Bifunctor
import Data.ByteString
import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time
import Data.Typeable
import Data.Word
import Env
import GHC.Generics
import Network.Socket (HostName, PortNumber)
import Text.Read (readMaybe)

data ConnectionInfo = ConnectionInfo
  { connectHost :: Text
  , connectPort :: Word16
  , connectUser :: Text
  , connectPassword :: Text
  , connectDatabase :: Text
  , sslMode :: Text
  }
  deriving (Eq, Generic, Read, Show, Typeable)

data LoggingDestination
  = -- | Logs are printed on the standard output
    StdOut
  | -- | Logs are printed on the standard output in JSON format
    Json
  | -- | Logs are sent to a file as JSON
    JSONFile
  deriving (Generic, Show)

-- | MLTP stands for Metrics, Logs, Traces and Profiles
data MLTP = MLTP
  { prometheusEnabled :: Bool
  , logger :: LoggingDestination
  , zipkinEnabled :: Bool
  , zipkinHost :: Maybe HostName
  , zipkinPort :: Maybe PortNumber
  }
  deriving stock (Generic, Show)

-- | The datatype that is used to model the external configuration
data Masna3Config = Masna3Config
  { dbConfig :: PoolConfig
  , connectionInfo :: StrictByteString
  , domain :: Text
  , httpPort :: Word16
  , mltp :: MLTP
  , deployed :: Bool
  , publicKey :: PublicKey
  , secretKey :: SecretKey
  }
  deriving stock (Generic, Show)

data PoolConfig = PoolConfig
  { connectionTimeout :: NominalDiffTime
  , connections :: Int
  }
  deriving stock (Show)

data TestConfig = TestConfig
  { httpPort :: Word16
  , dbConfig :: PoolConfig
  , connectionInfo :: StrictByteString
  , mltp :: MLTP
  }
  deriving stock (Generic)

parseConnectionInfo :: Parser Error StrictByteString
parseConnectionInfo =
  var str "MASNA3_DB_CONNSTRING" (help "libpq-compatible connection string")

parsePoolConfig :: Parser Error PoolConfig
parsePoolConfig =
  PoolConfig
    <$> var timeout "MASNA3_DB_TIMEOUT" (help "Timeout for each connection")
    <*> var
      (int >=> nonNegative)
      "MASNA3_DB_POOL_CONNECTIONS"
      (help "Number of connections across all sub-pools")

parseMLTP :: Parser Error MLTP
parseMLTP =
  MLTP
    <$> switch "MASNA3_PROMETHEUS_ENABLED" (help "Is Prometheus metrics export enabled (default false)")
    <*> var loggingDestination "MASNA3_LOGGING_DESTINATION" (help "Where do the logs go")
    <*> switch "MASNA3_ZIPKIN_ENABLED" (help "Is Zipkin trace collection enabled? (default false)")
    <*> var (pure . Just <=< nonempty) "MASNA3_ZIPKIN_AGENT_HOST" (help "The hostname of the Zipkin collection agent" <> def Nothing)
    <*> var (pure . Just <=< auto) "MASNA3_ZIPKIN_AGENT_PORT" (help "The port of the Zipkin collection agent" <> def Nothing)

parsePort :: Parser Error Word16
parsePort = var port "MASNA3_HTTP_PORT" (help "HTTP port")

parseDomain :: Parser Error Text
parseDomain = var str "MASNA3_DOMAIN" (help "URL domain")

parseDeployed :: Parser Error Bool
parseDeployed =
  var parseBool "MASNA3_DEPLOYED" (help "`true` if the service is deployed somewhere, `false` for local development / tests")

parsePublicKeyFromEnv :: Parser Error PublicKey
parsePublicKeyFromEnv =
  var readPublicKey "MASNA3_PUBLIC_KEY" (help "Public key for Biscuit authorisation")

parseSecretKeyFromEnv :: Parser Error SecretKey
parseSecretKeyFromEnv =
  var readSecretKey "MASNA3_SECRET_KEY" (help "Secret key for Biscuit authorisation")

parseConfig :: Parser Error Masna3Config
parseConfig =
  Masna3Config
    <$> parsePoolConfig
    <*> parseConnectionInfo
    <*> parseDomain
    <*> parsePort
    <*> parseMLTP
    <*> parseDeployed
    <*> parsePublicKeyFromEnv
    <*> parseSecretKeyFromEnv

parseTestConfig :: Parser Error TestConfig
parseTestConfig =
  TestConfig
    <$> parsePort
    <*> parsePoolConfig
    <*> parseConnectionInfo
    <*> parseMLTP

-- Env parser helpers

int :: Reader Error Int
int i = case readMaybe i of
  Nothing -> Left . unread . show $ i
  Just i' -> Right i'

port :: Reader Error Word16
port p = case int p of
  Left err -> Left err
  Right intPort ->
    if intPort >= 1 && intPort <= 65535
      then Right $ fromIntegral intPort
      else Left . unread $ p

parseBool :: Reader Error Bool
parseBool p = case Text.toLower (Text.pack p) of
  "true" -> Right True
  "false" -> Right False
  e -> Left $ unread (Text.unpack e)

nonNegative :: Int -> Either Error Int
nonNegative nni = if nni >= 0 then Right nni else Left . unread . show $ nni

timeout :: Reader Error NominalDiffTime
timeout t = second fromIntegral (int >=> nonNegative $ t)

loggingDestination :: Reader Error LoggingDestination
loggingDestination "stdout" = Right StdOut
loggingDestination "json" = Right Json
loggingDestination "json-file" = Right JSONFile
loggingDestination e = Left $ unread e

readPublicKey :: Reader Error PublicKey
readPublicKey s = maybeToRight (unread "Could not read public key") $ parsePublicKeyHex (BS8.pack s)

readSecretKey :: Reader Error SecretKey
readSecretKey s = maybeToRight (unread "Could not read secret key") $ parseSecretKeyHex (BS8.pack s)
