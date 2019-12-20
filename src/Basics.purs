module Basics where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console as Console

asyncInt :: Aff Int
asyncInt = pure 1

main :: Effect Unit
main =
  launchAff_ do
    Console.logShow =<< asyncInt
