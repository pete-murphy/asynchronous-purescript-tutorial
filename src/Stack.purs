module Stack where

import Prelude
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Parallel (parSequence)
import Data.Foldable (sum)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console as Console

type ErrorAff a
  = ExceptT String Aff a

slowInt :: Int -> ErrorAff Int
slowInt int = do
  _ <- liftAff do delay (Milliseconds 1000.0)
  if int > 10 then
    throwError "Too big!"
  else
    pure int

slowAdd :: Int -> Int -> ErrorAff Int
slowAdd a b = sum <$> parSequence [ slowInt a, slowInt b ]

main :: Effect Unit
main =
  launchAff_ do
    res1 <- runExceptT (slowAdd 1 2)
    Console.logShow res1
    res2 <- runExceptT (slowAdd 10 12)
    Console.logShow res2
