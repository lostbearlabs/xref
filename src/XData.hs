module XData (module XData) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
-- import qualified Data.ByteString.Builder as B
-- import System.IO
import GHC.Generics

-----------------
-- Def
-- Where a symbol is defined.

data Def = Def
  { symbol :: String,
    file :: String
  }
  deriving (Show, Eq, Generic)

instance ToJSON Def

instance FromJSON Def

-----------------
-- Ref
-- Where a symbol is referenced.

data Ref = Ref
  { targetSymbol :: String,
    referringSymbol :: Maybe String,
    referringFile :: String
  }
  deriving (Show, Eq, Generic)

instance ToJSON Ref

instance FromJSON Ref

-----------------
-- Database
-- A full set of Defs and Refs for a project

data Database = Database
  { defs :: [Def],
    refs :: [Ref]
  }
  deriving (Show, Eq, Generic)

-----------------
-- Serialization

instance ToJSON Database

instance FromJSON Database

writeDatabaseToFile :: FilePath -> Database -> IO ()
writeDatabaseToFile filePath db = do
  let jsonStr = encode db
  BS.writeFile filePath jsonStr

readDatabaseFromFile :: FilePath -> IO (Maybe Database)
readDatabaseFromFile filePath = do
  jsonStr <- BS.readFile filePath
  case eitherDecode jsonStr of
    Left err -> do
      putStrLn $ "Error decoding JSON: " ++ err
      return Nothing
    Right def -> return (Just def)

-----------------
-- A sample DB for testing
sampleDb :: Database
sampleDb =
  Database
    [ Def "a" "fileA",
      Def "b" "fileB"
    ]
    [ Ref "a" (Just "x") "fileX",
      Ref "d" Nothing "fileY"
    ]
