module Main where

import Data.Argonaut
import Data.Either (either)
import Effect.Promise (class Deferred, Promise)
import Prelude (Unit)
import Console as Console
import Decoders as Decoders
import EventHandler (handleEvent)

handleIncoming :: Deferred => String -> Json -> Promise Unit
handleIncoming msgType json = result
  where
  errorOrIntent = Decoders.decodeIncoming msgType json

  result =
    either
      (\msg -> Console.error msg json)
      handleEvent
      errorOrIntent
