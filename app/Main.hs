{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector          as V
import GHC.Generics
import Data.Csv
-- import Text.Email.Validate

data Student where
  Student :: {uid :: String, grade :: Float} -> Student
  deriving (Show, Eq, Generic)

data GradeBookEntry where
  GradeBookEntry :: {username :: Maybe String, name :: Maybe String, sid :: Maybe Integer, section :: Maybe String, pts :: Float} -> GradeBookEntry
  deriving (Show, Eq, Generic)

instance FromRecord Student
instance ToRecord   Student

instance FromNamedRecord Student where
    parseNamedRecord r =
      Student
      <$> r .: "Username"
      <*> r .: "Total Points"

instance FromNamedRecord GradeBookEntry where
  parseNamedRecord r =
    GradeBookEntry
    <$> r .: "SIS Login ID"
    <*> r .: "Student"
    <*> r .: "ID"
    <*> r .: "Section"
    <*> return 0

instance ToNamedRecord GradeBookEntry where
  toNamedRecord :: GradeBookEntry -> NamedRecord
  toNamedRecord (GradeBookEntry un n si sc pt) =
    namedRecord [
    "Student" .= n,
    "ID" .= si,
    "SIS Login ID" .= un,
    "Section" .= sc,
    "Grade" .= pt
    ]

parse :: FromNamedRecord a => BL.ByteString -> V.Vector a
parse s =
  case decodeByName s of
    Left  _       -> V.empty
    Right (_ , v) -> v

main :: IO()
main =
  do
    autograderCSVData <- BL.readFile "in.csv"
    do V.forM_ (parse autograderCSVData) $
         \ p ->
           putStrLn $ uid p ++ " scores " ++ show (grade p)
    templateData <- BL.readFile "template.csv"
    do V.forM_ (parse templateData) $
         \ p ->
           putStrLn $ show (username p) ++ " scores " ++ show (pts p)
    -- f <- BL.readFile "template.csv"
    -- case decodeByName f of
    --     Left err      -> print err
    --     Right (_, xs) -> V.forM_ xs $ \(GradeBookEntry n _ _ _ _) -> print n
