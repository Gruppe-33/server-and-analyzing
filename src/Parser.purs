module Parser where

import Prelude

import Control.Apply (lift2)
import Control.Lazy (class Lazy)
import Control.Plus (class Plus)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State.Class (class MonadState, gets, put)
import Control.MonadPlus (class Alt, class Alternative, class MonadZero, class MonadPlus, (<|>), guard, empty)
import Data.Either (either)
import Data.Traversable (traverse)
import Data.Array (many, some)
import Data.Char.Unicode (isSpace)
import Data.Compactable (class Compactable, separate, compact)
import Data.Filterable (class Filterable, eitherBool, maybeBool)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.CodeUnits (contains, fromCharArray, toCharArray, uncons)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)


withString :: (Array Char -> Array Char) -> String -> String
withString f = fromCharArray <<< f <<< toCharArray

withStringF
  :: forall f
   . Functor f
  => (Array Char -> f (Array Char)) -> String -> f String
withStringF f str = fromCharArray <$> f (toCharArray str)


type ParserResult a = { rest :: String, result :: a }

newtype Parser a = Parser (String -> Maybe (ParserResult a))

derive instance newtypeParser :: Newtype (Parser a) _

runParser :: forall a. Parser a -> String -> Maybe (ParserResult a)
runParser (Parser p) = p

instance functorParser :: Functor Parser where
  map = liftM1

instance applyParser :: Apply Parser where
  apply = ap

instance applicativeParser :: Applicative Parser where
  pure x = Parser \rest -> Just { rest, result: x }

instance bindParser :: Bind Parser where
  bind (Parser a) aPb = Parser \input -> do
    { rest: input', result: a' } <- a input
    runParser (aPb a') input'

instance monadParser :: Monad Parser

instance monadZeroParser :: MonadZero Parser

instance monadPlusParser :: MonadPlus Parser

instance monadStateParser :: MonadState String Parser where
  state = map (\(Tuple r s) -> Just { rest: s, result: r }) >>> Parser

instance monadRecParser :: MonadRec Parser where
  tailRecM f a = Parser \s -> tailRecM f' { rest: s, result: a }
    where
    f' { rest: s, result: a' } =
      case f a' of Parser st ->
        st s >>= \{ rest: s1, result: m } ->
          Just case m of
            Loop x -> Loop { rest: s1, result: x }
            Done y -> Done { rest: s1, result: y }

instance monadThrowParser :: MonadThrow Unit Parser where
  throwError _ = empty

instance monadErrorParser :: MonadError Unit Parser where
  catchError (Parser p) catch = Parser \input -> case p input of
    Just result -> Just result
    Nothing     -> runParser (catch unit) input

instance altParser :: Alt Parser where
  alt (Parser p1) (Parser p2) = Parser $ (<|>) <$> p1 <*> p2

instance plusParser :: Plus Parser where
  empty = Parser $ const Nothing

instance alternativeParser :: Alternative Parser

instance semigroupParser :: (Semigroup a) => Semigroup (Parser a) where
  append = lift2 append

instance monoidParser :: Monoid a => Monoid (Parser a) where
  mempty = pure mempty

instance compactableParser :: Compactable Parser where
  compact p = p >>= case _ of
    Just r -> pure r
    Nothing -> empty
  separate p =
    { left: p >>= either pure (const empty)
    , right: p >>= either (const empty) pure
    }

instance filterableParser :: Filterable Parser where
  partitionMap pred = map pred >>> separate
  partition pred p = { no: left, yes: right }
    where { left, right } = p # map (eitherBool pred) >>> separate
  filterMap pred = map pred >>> compact
  filter pred = map (maybeBool pred) >>> compact

instance lazyParser :: Lazy (Parser a) where
  defer p1 = Parser \input -> runParser (p1 unit) input


unconsP :: Parser Char
unconsP = gets uncons >>= case _ of
  Just { head, tail } -> put tail $> head
  Nothing             -> empty

parseIf :: (Char -> Boolean) -> Parser Char
parseIf p = do
  char <- unconsP
  guard (p char)
  pure char

charP :: Char -> Parser Char
charP = parseIf <<< eq

nChars :: Int -> Parser String
nChars n = replicateA n unconsP <#> fromCharArray

stringP :: String -> Parser String
stringP = withStringF $ traverse charP

spanP :: (Char -> Boolean) -> Parser String
spanP pred = many (parseIf pred) <#> fromCharArray

spanP1 :: (Char -> Boolean) -> Parser String
spanP1 pred = some (parseIf pred) <#> fromCharArray

ws :: Parser String
ws = spanP isSpace

untilChar :: Char -> Parser String
untilChar char = spanP (_ /= char)

containsStr :: String -> Parser Boolean
containsStr compareStr = gets $ contains (Pattern compareStr)

escapeChar :: Parser Char
escapeChar = foldl (<|>) empty escapeChars
  where
    toParser (Tuple from to) = stringP from $> to
    escapeChars = toParser <$>
      [ Tuple "\\\"" '"'
      , Tuple "\\'" '\''
      , Tuple "\\\\" '\\'
      , Tuple "\\n" '\n'
      , Tuple "\\r" '\r'
      , Tuple "\\t" '\t'
      ]

normalChar :: Parser Char
normalChar = parseIf ((&&) <$> notEq '"' <*> notEq '\\')

stringLiteral :: Parser String
stringLiteral = charP '"' *> string <* charP '"'
  where
    string = fromCharArray <$> many (normalChar <|> escapeChar)