module Sync where

import Prelude
import Effect (Effect)
import Effect.Timer (setTimeout)
import Effect.Class.Console as Console

slowInt :: Int -> (Int -> Effect Unit) -> Effect Unit
slowInt int cb = unit <$ setTimeout 1000 (cb int)

slowAdd :: Int -> Int -> (Int -> Effect Unit) -> Effect Unit
slowAdd a b cb =
  slowInt a \a' ->
    slowInt b \b' ->
      cb (a' + b')

main :: Effect Unit
main =
  slowAdd 1 2
    Console.logShow
