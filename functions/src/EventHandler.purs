module EventHandler where

import Effect.Promise

import Console (info)
import Data.Argonaut (encodeJson)
import DataTypes (AgentState, Message(..), tsToInstant)
import DateFormatting (Instant)
import Persistence (addUserlogEntry, updateUser)
import Prelude (Unit, unit, bind, pure)
import Slack (userInfo, viewPublish)

handleEvent :: Deferred => Message -> Promise Unit
handleEvent NoOp = pure unit

handleEvent (ChatMessage record) = do
  _ <- info "Message" (encodeJson record)
  _ <- addUserlogEntry record.userId { userId: record.userId, text: record.text, posix: tsToInstant record.ts }
  pure unit

handleEvent (AppHomeOpened record) = do
  _ <- info "AppHomeOpened" (encodeJson record)
  user <- userInfo record.user
  _ <- updateUser { userId: record.user, name: user.name, channel: record.channel }
  _ <- viewPublish record.user
  pure unit

handleEvent (Tick t) = do
  agentState <- 

handleTick :: Deferred => Instant -> AgentState -> Promise Unit
handleTick tick state = pure unit
