module CSVParser where

import Prelude

import Control.Monad.State.Class (gets)
import Control.MonadZero (empty)
import Data.Filterable (filterMap)
import Data.Int (fromString) as I
import Data.List (List(..), many, snoc, tail, (:))
import Data.Maybe (Maybe(..))
import Data.Number (fromString) as N
import Data.Traversable (traverse)
import Data.Unfoldable (replicateA)
import Parser (Parser, charP, spanP)


parseCSVBasic :: Parser (List (List String))
parseCSVBasic
   = many parseLine
  <* whenM (gets $ notEq "") empty

parseUntilBreak :: Parser String
parseUntilBreak = spanP case _ of
  ','  -> false
  '\n' -> false
  _    -> true

parseLine :: Parser (List String)
parseLine = snoc
  <$> many do parseUntilBreak <* charP ','
  <*>      do parseUntilBreak <* charP '\n'

parseLineFixed :: Int -> Parser (List String)
parseLineFixed length = snoc
  <$> replicateA (length - 1) do parseUntilBreak <* charP ','
  <*>                         do parseUntilBreak <* charP '\n'

parseCSVFixed :: Int -> Parser (List (List String))
parseCSVFixed length
   = many (parseLineFixed length)
  <* whenM (gets $ notEq "") empty

parseHResult :: Parser (List { h :: Int, result :: Int })
parseHResult = parseCSVFixed 2
  # filterMap tail
  # (=<<) do traverse listToRecord
    where
      listToRecord :: List String -> Parser { h :: Int, result :: Int }
      listToRecord (h' : result' : Nil) =
        case I.fromString h', I.fromString result' of
          Just h, Just result -> pure { h, result }
          _, _ -> empty
      listToRecord _ = empty

parseAnalyzedResult :: Parser (List
  { h :: Int
  , difference :: Int
  , average :: Number
  , average3 :: Number
  , amount :: Int
  })
parseAnalyzedResult = parseCSVFixed 5
  # filterMap tail
  # (=<<) do traverse listToRecord
    where
      listToRecord :: List String -> Parser
        { h :: Int
        , difference :: Int
        , average :: Number
        , average3 :: Number
        , amount :: Int
        }
      listToRecord (h' : difference' : average' : average3' : amount' : Nil) =
        case traverse I.fromString [h', difference', amount']
           , traverse N.fromString [average', average3']
        of
          Just [h, difference, amount], Just [average, average3] ->
            pure { h, difference, average, average3, amount }
          _, _ -> empty
      listToRecord _ = empty
