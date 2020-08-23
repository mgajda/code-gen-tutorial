{-# language DeriveGeneric #-}
module Main where

{- Read inputs from "input*.csv" files
 - and generate parsers for "example.csv"
 -}

import           Control.Monad as M
import           Data.Csv
import           GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Vector as V
import           System.Environment
import           Language.Haskell.RunHaskellModule

{-
Field, Type, Parser
Name, String, id
Age, Int, readEither
Date, DayOfWeek, readEither
 -}
data SchemaRecord = SchemaRecord {
    field  :: String
  , aType  :: String
  , parser :: String
  } deriving (Generic, Show)

instance ToRecord SchemaRecord where

instance FromRecord SchemaRecord where

showSchema = V.mapM_ (print :: SchemaRecord -> IO ())

genSchema schema = do
  showSchema schema
  writeFile "Schema.hs" $ generatedCode schema

generatedCode :: V.Vector SchemaRecord -> String
generatedCode schema = prologue <> dataRecord schema <> epilogue

dataRecord _ = unlines [
   "data InputRecord = InputRecord {"
  , "    name :: String" -- this should customized
  , "  , age :: Int" -- this should be customized
  , "  , day :: DayOfWeek" -- this should be customized
  , "} deriving (Generic, Show)"
  ]

prologue = unlines [
                    "{-# language DeriveGeneric #-}"
                   ,"module Parser where"
                   ,"import Data.Csv"
                   ,"import qualified Data.Vector as V"
                   ,"import qualified Data.ByteString.Lazy.Char8 as BS"
                   ,"import GHC.Generics"
                   ,"import Control.Monad"
                   ,"import Data.Time.Calendar"
                   ]

epilogue = unlines [
    "instance ToRecord InputRecord where"
  , "instance FromRecord InputRecord where"
  , "main = do"
  , "input <- BS.readFile \"test/example.csv\""
  , "either putStrLn (V.mapM_ (print :: InputRecord -> IO ())) $ decode HasHeader input"
  ]

main :: IO ()
main = do
  inputFilenames <- getArgs
  forM_ inputFilenames $ \filename -> do
    putStrLn $ filename <> ":"
    input <- BS.readFile filename
    either putStrLn genSchema $ decode HasHeader input
    runHaskellModule "Schema.hs" []

