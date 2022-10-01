module Main where

import           Aws.Lambda ( defaultDispatcherOptions
                            , addStandaloneLambdaHandler
                            , runLambdaHaskellRuntime )
import qualified Lib

main :: IO ()
main =
  runLambdaHaskellRuntime
    defaultDispatcherOptions
    (pure ())
    id
    (addStandaloneLambdaHandler "qvf-generate-scripts" Lib.handler)