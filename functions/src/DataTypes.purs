module DataTypes where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Int (floor)
import Data.Maybe (Maybe)
import Data.String (drop)
import Data.String.CodeUnits (dropWhile, takeWhile)
import DateFormatting (Instant, instant)
import Global (readInt)
import Util ((|>))

newtype UserId
  = UserId String

tsToInstant :: TS -> Instant
tsToInstant (TS { secs, nanos }) = instant (floor secs * 1000)

newtype TS
  = TS
  { secs :: Number
  , nanos :: Number
  }

type User
  = { userId :: UserId
    , name :: String
    , channel :: String
    }

type UserlogEntry
  = { userId :: UserId
    , posix :: Instant
    , text :: String
    }

data Message
  = ChatMessage
    { text :: String
    , userId :: UserId
    , channelId :: String
    , ts :: TS
    , botId :: Maybe String
    }
  | AppHomeOpened
    { user :: UserId
    , channel :: String
    , tab :: String
    }
  | Tick { posix :: Instant }

data AgentState
  = AwaitingMorningGreeting
  | AwaitingAfternoonReminder

instance showUserId :: Show UserId where
  show (UserId id) = "UserId(" <> id <> ")"

instance userIdDecoder :: DecodeJson UserId where
  decodeJson json = UserId <$> decodeJson json

instance userIdEncoder :: EncodeJson UserId where
  encodeJson (UserId id) = encodeJson id

instance instantDecoder :: DecodeJson TS where
  decodeJson json = do
    nano <- decodeJson json |> map (dropWhile (_ /= '.')) |> map (drop 1) |> map (readInt 10)
    secs <- decodeJson json |> map (takeWhile (_ /= '.')) |> map (readInt 10)
    pure (TS { secs: secs, nanos: nano })

instance instantEncoder :: EncodeJson TS where
  encodeJson (TS record) = encodeJson ((show record.secs) <> (show record.nanos))
