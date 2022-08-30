module Main where

import Aws.Lambda
import qualified Lib

main :: IO ()
main =
  runLambdaHaskellRuntime
    defaultDispatcherOptions
    (pure ())
    id
    (addStandaloneLambdaHandler "handler" Lib.handler)