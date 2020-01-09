module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.DateTime.Instant (toDateTime)
import Data.Either (either)
import Data.Foldable (fold)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format)
import Data.Function.Uncurried (Fn3)
import Data.List (List(..), (:))
import Data.Semigroup.Foldable (intercalateMap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Effect.Ref as R
import Foreign (ForeignError(..), MultipleErrors, fail, renderForeignError)
import Node.Encoding (Encoding(..))
import Node.Express.App (App, get, listenHttp, post, use, useExternal)
import Node.Express.Handler (Handler)
import Node.Express.Middleware.Static (static)
import Node.Express.Request (getBody)
import Node.Express.Response (send)
import Node.Express.Types (Request, Response)
import Node.FS.Sync (writeTextFile, exists, mkdir)
import Simple.JSON as JSON
import Types (Instruction, Result, ResultType, getOne, noInstruction, resultTypeHandlers, printResult)

foreign import textBodyParser
  :: Fn3 Request Response (Effect Unit) (Effect Unit)

errorHandler :: MultipleErrors -> Handler
errorHandler errors = (*>)
  <$> do \e -> send $ "There was a problem with your request: \n" <> e
  <*> do \e -> log $ "Someone sent a result, but something went wrong: \n" <> e
  $ intercalateMap "\n" renderForeignError errors

resultHandler :: Tuple Result Int -> Handler
resultHandler (Tuple result t) = do
  log $ "Received result of type " <> show t <> "."
  send "Successfully received results!"
  liftEffect do
    time <- now <#> (toDateTime >>> format dateFormatter)
    let (Tuple t content) = printResult result
    mkdir "data"
      # unlessM (exists "data")
    flip tailRecM 1 \n -> ifM
      do exists $ fileName time n t
      do pure (Loop $ n + 1)
      do writeTextFile UTF8 (fileName time n t) content <#> Done

dateFormatter :: Formatter
dateFormatter =
  ( YearFull
  : Placeholder "-"
  : MonthTwoDigits
  : Placeholder "-"
  : DayOfMonthTwoDigits
  : Placeholder " "
  : Hours24
  : Placeholder ":"
  : MinutesTwoDigits
  : Placeholder ":"
  : SecondsTwoDigits
  : Nil)
--
fileName :: String -> Int -> Int -> String
fileName time n t = fold
  [ "data/"
  , show t
  , "_"
  , time
  , if n == 1
    then ""
    else " " <> show n
  , ".csv"
  ]

postResultHandler :: Handler
postResultHandler = do
  bodyE <- getBody
  resultE <- pure do
    bodyText <- bodyE
    resultType <- readType bodyText
    result <- foldlWithIndex
      do \t acc f -> if t == resultType then f bodyText else acc
      do fail $ ForeignError $ "Invalid result type: " <> show resultType
      resultTypeHandlers
    pure $ Tuple result resultType
  either errorHandler resultHandler (runExcept resultE)
  where
    readType b = JSON.readJSON' b <#> (\t -> (t :: ResultType).type)

postInstructionHandler :: R.Ref (Array Instruction) -> Handler
postInstructionHandler ref = do
  bodyE <- getBody
  resultE <- pure do
    bodyText <- bodyE
    JSON.readJSON' bodyText
  either
    errorHandler
    (\instruction -> do
      liftEffect $ R.modify_ (_ <> [instruction]) ref
      log $ "Received instruction: " <> show instruction
      send $ "Successfully received instruction: " <> show instruction)
    (runExcept resultE)

getInstructionHandler :: R.Ref (Array Instruction) -> Handler
getInstructionHandler ref = do
    result <- liftEffect $ R.modify' (getOne noInstruction) ref
    log ("Someone fetched this instruction: " <> show result.element)
      # when result.changed
    send $ JSON.writeJSON result.element

app :: R.Ref (Array Instruction) -> App
app ref = do
  use $ static "src/static"
  useExternal textBodyParser
  get "/getInstruction" (getInstructionHandler ref)
  post "/postResult" postResultHandler
  post "/postInstruction" (postInstructionHandler ref)

main :: Effect Unit
main = do
  ref <- R.new ([] :: Array Instruction)
  void $ listenHttp (app ref) 8080 \_ -> log "Server started."