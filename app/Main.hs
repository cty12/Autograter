{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector          as V
import GHC.Generics
import Data.Csv
import Text.Email.Validate

data Student where
  Student :: {uid :: String, grade :: Float} -> Student
  deriving (Show, Eq, Generic)

data GradeBookEntry where
  GradeBookEntry :: {student :: Student, name :: String, sid :: Integer, section :: String} -> GradeBookEntry
  deriving (Show, Eq, Generic)

instance FromRecord Student
instance ToRecord   Student

instance FromNamedRecord Student where
    parseNamedRecord r =
      Student
      <$> r .: "Username"
      <*> r .: "Total Points"

instance ToNamedRecord GradeBookEntry where
  toNamedRecord :: GradeBookEntry -> NamedRecord
  toNamedRecord (GradeBookEntry s n si sc) =
    namedRecord [
    "Student" .= n,
    "ID" .= si,
    "SIS Login ID" .= uid s,
    "Section" .= sc
    ]

-- Parse Autograder CSV file
parseAutograder :: BL.ByteString -> V.Vector Student
parseAutograder s =
  case decodeByName s of
    Left  _       -> V.empty
    Right (_ , v) -> v

main :: IO()
main =
  do
    csvData <- BL.readFile "in.csv"
    do V.forM_ (parseAutograder csvData) $
         \ p ->
           putStrLn $ uid p ++ " scores " ++ show (grade p)
