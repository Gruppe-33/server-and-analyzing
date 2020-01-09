module Types where

import Prelude

import Color (rgb, Color)
import Data.Array (head, tail)
import Data.Foldable (intercalate)
import Data.List (List(..), (:))
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy(..), Variant, case_, inj, on)
import Foreign (F)
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Cons)
import Simple.JSON (class ReadForeign)
import Simple.JSON as JSON

type Instruction =
  { type :: Int
  , name :: Maybe String
  , payload :: Maybe (Array Int)
  }

type State =
  { instructions :: Array Instruction
  , nextId :: Int
  }

noInstruction :: Instruction
noInstruction = { type: 0, name: Nothing, payload: Nothing }

type ResultType = { type :: Int }

type ResultId = { id :: Int }

type HSVResult = Array { h :: Int, result :: Int }
type LightResult = Array { b :: Int, result :: Int }
type CustomResult = Array { r :: Int, g :: Int, b :: Int, result :: Int }

printResultHelper
  :: forall r. String -> (r -> String) -> Array r -> String
printResultHelper topline showR results
   = topline
  <> "\n"
  <> (results <#> showR # intercalate "\n")
  <> "\n"

printHSVResult :: HSVResult -> Tuple Int String
printHSVResult = Tuple 1 <<< printResultHelper "h,result" showR
  where showR { h, result } = intercalate "," $ show <$> [h, result]

printLightResult :: LightResult -> Tuple Int String
printLightResult = Tuple 2 <<< printResultHelper "b,result" showR
  where showR { b, result } = intercalate "," $ show <$> [b, result]

printCustomResult :: CustomResult -> Tuple Int String
printCustomResult = Tuple 3 <<< printResultHelper "r,g,b,result" showR
  where showR { r, g, b, result } = intercalate "," $ show <$> [r, g, b, result]

type Result = Variant
  ( hsv :: HSVResult
  , light :: LightResult
  , custom :: CustomResult
  )

printResult :: Result -> Tuple Int String
printResult = case_
  # on (SProxy :: SProxy "hsv")    printHSVResult
  # on (SProxy :: SProxy "light")  printLightResult
  # on (SProxy :: SProxy "custom") printCustomResult

resultTypeHandlers :: M.Map Int (String -> F Result)
resultTypeHandlers = M.fromFoldable
  ( Tuple 1 (read (SProxy :: SProxy "hsv"))
  : Tuple 2 (read (SProxy :: SProxy "light"))
  : Tuple 3 (read (SProxy :: SProxy "custom"))
  : Nil)

optimalColors :: M.Map String Color
optimalColors = M.fromFoldable
  ( Tuple "solbærsirup" (rgb 0 84 255)
  : Nil)

getData :: forall a. { data :: a } -> a
getData = _.data

read :: forall r2 r1 a sym
      . Cons sym a r1 r2 => IsSymbol sym => ReadForeign a
     => SProxy sym
     -> String
     -> F (Variant r2)
read proxy = JSON.readJSON' >>> map (getData >>> inj proxy)


getOne :: forall a. a -> Array a ->
  { state :: Array a
  , value ::
    { element :: a
    , changed :: Boolean
    }
  }
getOne default [] = { state: [], value: { element: default, changed: false } }
getOne _ xs =
  { state: unsafeTail xs
  , value:
    { element: unsafeHead xs
    , changed: true
    }
  }
  where
    unsafeFromJust :: forall b. Maybe b -> b
    unsafeFromJust = unsafePartial fromJust

    unsafeTail = unsafeFromJust <<< tail
    unsafeHead = unsafeFromJust <<< head