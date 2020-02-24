module Decoders where

import Data.Either (Either(..))
import Data.Argonaut
import DataTypes (Message(..))
import Prelude (map, (<>), bind, pure)
import Util ((|>))

decodeIncoming :: String -> Json -> Either String Message
decodeIncoming "app_home_opened" json = decodeJson json |> map AppHomeOpened

decodeIncoming "message" json = do
  obj <- decodeJson json
  text <- getField obj "text"
  userId <- getField obj "user"
  channelId <- getField obj "channel"
  ts <- getField obj "ts"
  botId <- getFieldOptional obj "bot_id"
  pure (ChatMessage { text: text, userId: userId, channelId: channelId, botId: botId, ts: ts })

decodeIncoming "error" json = Left (stringify json)

decodeIncoming any value = Left ("No handler for message of type " <> any)
