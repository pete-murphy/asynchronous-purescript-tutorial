module Async where

import Prelude
import Control.Apply (lift2)
import Data.Function (on)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class.Console as Console

slowInt :: Int -> Aff Int
slowInt int = delay (Milliseconds 1000.0) *> pure int

slowAdd :: Int -> Int -> Aff Int
slowAdd = on (lift2 (+)) slowInt

main :: Effect Unit
main =
  launchAff_ do
    slowAdd 1 2 >>= Console.logShow
