module Race where

import Prelude
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Parallel (parOneOf, parSequence)
import Data.Foldable (sum)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Random (random)

type ErrorAff a
  = ExceptT String Aff a

slowInt :: Int -> ErrorAff Int
slowInt int = do
  d <- liftAff do liftEffect random
  _ <- liftAff do delay (Milliseconds (d * 100.0))
  if int > 10 then
    throwError "Too big!"
  else
    pure int

slowAdd :: Int -> Int -> ErrorAff Int
slowAdd a b = sum <$> parSequence [ slowInt a, slowInt b ]

main :: Effect Unit
main =
  launchAff_ do
    Console.logShow
      =<< parOneOf
          [ runExceptT (slowAdd 11 2)
          , runExceptT (slowAdd 1 1)
          ]
