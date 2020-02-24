module EventHandler where

import Effect.Promise
import Console (info)
import Data.Argonaut (encodeJson)
import DataTypes (Message(..), tsToInstant)
import Persistence (addUserlogEntry, updateUser)
import Prelude (Unit, unit, bind, pure)
import Slack (userInfo, viewPublish)

handleEvent :: Deferred => Message -> Promise Unit
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

handleEvent (Tick t) = pure unit
