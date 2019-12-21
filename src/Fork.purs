module Fork where

import Prelude
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, joinFiber, launchAff_)
import Effect.Class.Console as Console

slowInt :: Int -> Aff Int
slowInt int = delay (Milliseconds 1000.0) *> pure int

slowAdd :: Int -> Int -> Aff Int
slowAdd a b = do
  fiberA <- forkAff (slowInt a)
  fiberB <- forkAff (slowInt b)
  slowA <- joinFiber fiberA
  slowB <- joinFiber fiberB
  pure (slowA + slowB)

main :: Effect Unit
main =
  launchAff_ do
    slowAdd 1 2 >>= Console.logShow
