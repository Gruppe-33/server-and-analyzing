module Analyze2 where

import Prelude

import CSVParser (parseAnalyzedResult)
import Data.Foldable (fold, maximum, minimum, sum)
import Data.Int (toNumber)
import Data.List (List(..), fromFoldable, groupBy, sortBy, (!!), (..), (:))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (for, sequence, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), curry)
import Effect (Effect)
import Global (toFixed)
import Math (abs)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Parser (runParser)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = do
  results <- for toCompare \files -> ado
    compared <- for files \file -> ado
      content <- readTextFile UTF8 file
      in runParser parseAnalyzedResult content <#> _.result
    in sequence compared >>= analyzeSeries
  void $ sequence results
    # (map <<< map) printResults
    # (traverse <<< traverseWithIndex) (curry writeCSVFile)

toCompare :: M.Map String (List String)
toCompare = M.fromFoldable
  ( Tuple "iste"   ("0" : "i25" : "i50" : "i75" : "i100" : Nil)
  : Tuple "solbær" ("0" : "s25" : "s50" : "s75" : "s100" : Nil)
  : Nil) <#> map \s -> "data_/" <> s <> ".csv"

writeCSVFile :: Tuple String String -> Effect Unit
writeCSVFile (Tuple name content) = writeTextFile UTF8
  do "data_/" <> name <> ".csv"
  content

printResults :: List
  { h :: Int
  , averageIndividualDifference :: Number
  , averageIndividualDifferencePts :: Number
  , totalSpread :: Number
  , totalSpreadPts :: Number
  , spreadDifference :: Number
  , spreadDifferencePts :: Number
  } -> String
printResults results
   = "h,averageIndividualDifference,averageIndividualDifferencePts,"
  <> "totalSpread,totalSpreadPts,spreadDifference,spreadDifferencePts\n"
  <> fold (results <#> printResult1)

printResult1 ::
  { h :: Int
  , averageIndividualDifference :: Number
  , averageIndividualDifferencePts :: Number
  , totalSpread :: Number
  , totalSpreadPts :: Number
  , spreadDifference :: Number
  , spreadDifferencePts :: Number
  } -> String
printResult1
  { h
  , averageIndividualDifference, averageIndividualDifferencePts
  , totalSpread, totalSpreadPts
  , spreadDifference, spreadDifferencePts
  } = fold
    [ show h, ","
    , toFixed' averageIndividualDifference, ","
    , toFixed' averageIndividualDifferencePts, ","
    , toFixed' totalSpread, ","
    , toFixed' totalSpreadPts, ","
    , toFixed' spreadDifference, ","
    , toFixed' spreadDifferencePts, "\n"
    ]
      where toFixed' n = unsafePartial let (Just n') = toFixed 3 n in n'

analyzeSeries :: List (List
  { h :: Int
  , difference :: Int
  , average :: Number
  , average3 :: Number
  , amount :: Int
  }) -> Maybe (List
    { h :: Int
    , averageIndividualDifference :: Number
    , averageIndividualDifferencePts :: Number
    , totalSpread :: Number
    , totalSpreadPts :: Number
    , spreadDifference :: Number
    , spreadDifferencePts :: Number
    })
analyzeSeries series = join series
  # sortBy do \r1 r2 -> compare r1.h r2.h
  # groupBy do \r1 r2 -> r1.h == r2.h
  # map fromFoldable -- groupBy gives non-empty lists
  # map hueStats
  # map (map hueScores)
  # sequence
  # map do sortBy do \r1 r2 -> compare (totalPoints r2) (totalPoints r1)

totalPoints :: forall r.
  { averageIndividualDifferencePts :: Number
  , totalSpreadPts :: Number
  , spreadDifferencePts :: Number
  | r } -> Number
totalPoints
  { averageIndividualDifferencePts, totalSpreadPts, spreadDifferencePts }
  = averageIndividualDifferencePts + totalSpreadPts + spreadDifferencePts

hueStats :: List
  { h :: Int
  , difference :: Int
  , average :: Number
  , average3 :: Number
  , amount :: Int
  } -> Maybe
    { h :: Int
    , averageIndividualDifference :: Number
    , totalSpread :: Number
    , spreadDifference :: Number
    }
hueStats input@({h}:_) = do
  let values = input <#> _.average
      differences = input <#> _.difference <#> toNumber
      averageIndividualDifference = sum differences / 5.0
  spreads <- for (0 .. 3) \n -> do (-) >>> compose abs
    <$> values !! n
    <*> values !! (n + 1)
  let totalSpread = sum spreads
  spreadDifference <- (-) <$> maximum spreads <*> minimum spreads
  pure { h, averageIndividualDifference, totalSpread, spreadDifference }
hueStats _ = Nothing

hueScores ::
  { h :: Int
  , averageIndividualDifference :: Number
  , totalSpread :: Number
  , spreadDifference :: Number
  } ->
    { h :: Int
    , averageIndividualDifference :: Number
    , averageIndividualDifferencePts :: Number
    , totalSpread :: Number
    , totalSpreadPts :: Number
    , spreadDifference :: Number
    , spreadDifferencePts :: Number
    }
hueScores o@{ h, averageIndividualDifference, totalSpread, spreadDifference } =
  { averageIndividualDifferencePts:
    averageIndividualDifferencePoints averageIndividualDifference
  , totalSpreadPts: totalSpreadPoints totalSpread
  , spreadDifferencePts: spreadDifferencePoints spreadDifference
  , h
  , averageIndividualDifference
  , totalSpread
  , spreadDifference
  }

averageIndividualDifferencePoints :: Number -> Number
averageIndividualDifferencePoints = scale 0.0 35.0 10.0 0.0 >>> clamp 0.0 10.0

totalSpreadPoints :: Number -> Number
totalSpreadPoints = scale 0.0 511.0 0.0 10.0 >>> clamp 0.0 10.0

spreadDifferencePoints :: Number -> Number
spreadDifferencePoints = scale 0.0 200.0 10.0 0.0 >>> clamp 0.0 10.0

scale :: Number -> Number -> Number -> Number -> Number -> Number
scale lowIn highIn lowOut highOut n =
  (n - lowIn) / (highIn - lowIn) * (highOut - lowOut) + lowOut
