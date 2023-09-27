{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS
import qualified Data.Vector          as V
import qualified Data.Map             as M
import GHC.Generics
import Data.Csv
import Text.Email.Validate
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Options.Applicative (execParser)
import CmdArgs

data Student where
  Student :: {uid :: BS.ByteString, grade :: Float} -> Student
  deriving (Show, Eq, Generic)

data GradeBookEntry where
  GradeBookEntry ::
    {username :: Maybe BS.ByteString,
     fullname :: Maybe String,
     sid      :: Maybe Integer,
     section  :: Maybe String,
     pts      :: Float,
     project  :: BS.ByteString} -> GradeBookEntry
  deriving (Show, Eq, Generic)

instance FromRecord Student
instance ToRecord   Student

instance FromNamedRecord Student where
    parseNamedRecord r =
      Student
      <$> (do
           s <- r .: "Username"
           return (maybe s localPart (emailAddress s)))
      <*> r .: "Total Points"

instance FromNamedRecord GradeBookEntry where
  parseNamedRecord r =
    GradeBookEntry
    <$> r .: "SIS Login ID"
    <*> r .: "Student"
    <*> r .: "ID"
    <*> r .: "Section"
    <*> return 0
    <*> return ""

instance ToNamedRecord GradeBookEntry where
  toNamedRecord :: GradeBookEntry -> NamedRecord
  toNamedRecord (GradeBookEntry un fn si sc pt proj) =
    namedRecord [
    "Student" .= fn,
    "ID" .= si,
    "SIS Login ID" .= un,
    "Section" .= sc,
    proj .= pt
    ]

parse :: FromNamedRecord a => BL.ByteString -> (Header, V.Vector a)
parse s =
  case decodeByName s of
    Left  _       -> (V.empty, V.empty)
    Right (h , v) -> (h, v)

type GradeBook = V.Vector GradeBookEntry
type GradeMap  = M.Map BS.ByteString Float

buildGradeMap :: V.Vector Student -> GradeMap
buildGradeMap sts = M.fromList $ map (\ st -> (uid st, grade st)) (V.toList sts)

buildGradeBook :: GradeBook -> GradeMap -> BS.ByteString -> GradeBook
buildGradeBook (V.uncons -> Nothing) _ _ = V.empty
buildGradeBook (V.uncons -> Just (r, rs)) m proj =
  let rest = buildGradeBook rs m proj in
  maybe rest (\s -> V.cons (buildEntry r s proj) rest) (username r)
  where
  buildEntry rcd s pr = rcd { pts = fromMaybe 0 (M.lookup s m), project = pr }

findProjectName :: String -> Header -> Maybe BS.ByteString
findProjectName proj = V.find (BS.isPrefixOf $ fromString proj)

main :: IO()
main =
  do
    args <- execParser opts
    let inputPath = projectName args ++ ".csv"
    autograderCSVData <- BL.readFile inputPath
    let (_, agParsed) = parse autograderCSVData
    let m = buildGradeMap agParsed
    templateData <- BL.readFile (templatePath args)
    let (tempHead, tempParsed) = parse templateData
    maybe (fail "Project not found in gradebook template!")
      (\ proj ->
         let gradeBook = buildGradeBook tempParsed m proj in
         let s = encodeByName (V.fromList ["Student", "ID", "SIS Login ID", "Section", proj]) (V.toList gradeBook) in
         case outputPath args of
         StdOut       -> BL.putStr s
         OutputFile f -> BL.writeFile f s)
      (findProjectName (projectName args) tempHead)
