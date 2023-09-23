{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- from base
import GHC.Generics
import System.IO
import System.Exit (exitFailure)
-- from bytestring
import Data.ByteString (ByteString, hGetSome, empty)
import qualified Data.ByteString.Lazy as BL
-- from cassava
import Data.Csv.Incremental
import Data.Csv (FromRecord, ToRecord)

data Person = Person
  { name :: !ByteString
  , age  :: !Int
  } deriving (Show, Eq, Generic)

instance FromRecord Person
instance ToRecord Person

persons :: [Person]
persons = [Person "John Doe" 19, Person "Smith" 20]

writeToFile :: IO ()
writeToFile = do
  BL.writeFile "persons.csv" $ encode $
    foldMap encodeRecord persons

feed :: (ByteString -> Parser Person) -> Handle -> IO (Parser Person)
feed k csvFile = do
  hIsEOF csvFile >>= \case
    True  -> return $ k empty
    False -> k <$> hGetSome csvFile 4096

readFromFile :: IO ()
readFromFile = do
  withFile "persons.csv" ReadMode $ \ csvFile -> do
    let loop !_ (Fail _ errMsg) = do putStrLn errMsg; exitFailure
        loop acc (Many rs k)    = loop (acc <> rs) =<< feed k csvFile
        loop acc (Done rs)      = print (acc <> rs)
    loop [] (decode NoHeader)

main :: IO ()
main = do
  writeToFile
  readFromFile
