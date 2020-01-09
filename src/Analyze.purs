module Analyze where

import Prelude

import Control.MonadZero (guard)
import CSVParser (parseHResult)
import Data.Foldable (fold, length, maximum, minimum, sum, traverse_)
import Data.List (List(..), fromFoldable, groupBy, slice, sortBy, zip, (..), (:))
import Data.List.Lazy as L
import Data.List.ZipList (ZipList(..))
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.Traversable (sequence, for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Global (toFixed)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Parser (runParser)
import Partial.Unsafe (unsafePartial)


main :: Effect Unit
main = do
  results <- for folders \folder -> ado
    result <- for files \file -> ado
      content <- readTextFile UTF8 (concatPath folder file)
      in runParser parseHResult content <#> _.result
    in sequence result >>= analyze1
  sequence results
    # (map <<< map) printResults
    # map (zip folders)
    # (traverse_ <<< traverse_) writeCSVFile

printResults :: List
  { h :: Int
  , difference :: Int
  , average :: Number
  , average3 :: Number
  , amount :: Int
  } -> String
printResults results
   = "h,difference,average,average3,amount\n"
  <> fold (results <#> printResult1)

printResult1 ::
  { h :: Int
  , difference :: Int
  , average :: Number
  , average3 :: Number
  , amount :: Int
  } -> String
printResult1 { h, difference, average, average3, amount } = fold
  [ show h, ","
  , show difference, ","
  , toFixed' average, ","
  , toFixed' average3, ","
  , show amount, "\n"
  ]
    where toFixed' n = unsafePartial let (Just n') = toFixed 3 n in n'

writeCSVFile :: Tuple String String -> Effect Unit
writeCSVFile (Tuple name content) = writeTextFile UTF8
  do "data_/" <> name <> ".csv"
  content

concatPath :: String -> String -> String
concatPath folder file = "data_/" <> folder <> "/" <> file

folders :: List String
folders =
  ( "0" : "air"
  : "s25" : "s50" : "s75" : "s100"
  : "i25" : "i50" : "i75" : "i100"
  : Nil)

files :: List String
files = (1 .. 5) <#> \n -> show n <> ".csv"

analyze1 :: List (List { h :: Int, result :: Int }) -> Maybe (List
  { h :: Int
  , difference :: Int
  , average :: Number
  , average3 :: Number
  , amount :: Int
  })
analyze1 input = join input
  # sortBy do \r1 r2 -> compare r1.h r2.h
  # groupBy do \r1 r2 -> r1.h == r2.h
  # map fromFoldable
  # map calculate
  # sequence

-- | Calculate some values from a list of results with the same h value
calculate :: List { h :: Int, result :: Int } -> Maybe
  { h :: Int
  , difference :: Int
  , average :: Number
  , average3 :: Number
  , amount :: Int
  }
calculate input@({h}:_) = do
  let results = input <#> _.result
      intLen = length results
      len = toNumber intLen
      average = toNumber (sum results) / len
      average3 = toNumber (sum $ slice 1 (intLen - 1) results) / (len - 2.0)
  guard $ len > 0.0
  difference <- (-) <$> maximum results <*> minimum results
  pure { h, difference, average3, average, amount: intLen }
calculate _ = Nothing

toZipList :: List ~> ZipList
toZipList = L.fromFoldable >>> ZipList

fromZipList :: ZipList ~> List
fromZipList (ZipList l) = L.toUnfoldable l