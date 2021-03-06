module DataTypes where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, getField, getFieldOptional, jsonEmptyArray)
import Data.Either (Either)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (drop)
import Data.String.CodeUnits (dropWhile, takeWhile)
import DateFormatting (Instant, LocalTime, Weekday, atHour, atMillisecond, atMinute, atSecond, europe_oslo, findNextDayAfter, getHour, getMillisecond, getMinute, getSecond, getWeekday, instant, isWorkday, toInstant, toZonedDateTime)
import Global (readInt)
import Util ((|>))

newtype UserId
  = UserId String

tsToInstant :: TS -> Instant
tsToInstant (TS { secs, nanos }) = instant (secs * 1000.0)

newtype TS
  = TS
  { secs :: Number
  , nanos :: Number
  }

type User
  = { userId :: UserId
    , name :: String
    , channel :: String
    , active :: Boolean
    }

type UserlogEntry
  = { userId :: UserId
    , posix :: Instant
    , text :: String
    }

data Message
  = NoOp
  | ChatMessage
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
  | ActionMessage { id :: String, value :: String, userId::String }

data Action
  = Start {userId::String}
  | Stop {userId::String}
  | Clear {userId::String}

data TriggerSchedule
  = EveryWorkdayAt LocalTime
  | EveryGivenWeekday Weekday LocalTime

nextTriggerInstant :: Instant -> TriggerSchedule -> Instant
nextTriggerInstant now (EveryWorkdayAt scheduledTime) =
  let
    nowDT = (toZonedDateTime europe_oslo now)

    nextDt =
      nowDT
        |> findNextDayAfter (getWeekday >>> isWorkday)
        |> atHour (getHour scheduledTime)
        |> atMinute  (getMinute scheduledTime)
        |> atSecond  (getSecond scheduledTime)
        |> atMillisecond  (getMillisecond scheduledTime)
  in
    toInstant ( nextDt)

nextTriggerInstant now (EveryGivenWeekday day localTime) =
  let
    nowDT = toZonedDateTime europe_oslo now

    nextDt =
      findNextDayAfter (\ndc -> getWeekday ndc == day) nowDT
        |> atHour (getHour localTime)
        |> atMinute (getMinute localTime)
        |> atSecond (getSecond localTime)
        |> atMillisecond (getMillisecond localTime)
  in
    toInstant nextDt

type TriggerState
  = { nextInstant :: Instant
    , name :: String
    }

type Trigger
  = { schedule :: TriggerSchedule
    , name :: String
    }

type TriggerWithState
  = { schedule :: TriggerSchedule
    , name :: String
    , nextInstant :: Instant
    }

instance showUserId :: Show UserId where
  show (UserId id) = "UserId(" <> id <> ")"

instance userIdDecoder :: DecodeJson UserId where
  decodeJson json = UserId <$> decodeJson json

instance userIdEncoder :: EncodeJson UserId where
  encodeJson (UserId id) = encodeJson id

userDecoder :: Json -> Either String User 
userDecoder json = do
    obj <- decodeJson json
    userId <- getField obj "userId" 
    name <- getField obj "name" 
    channel <- getField obj "channel" 
    maybeActive <- getFieldOptional obj "active"
    pure {userId:userId,name:name,channel:channel,active:fromMaybe true maybeActive}

instance instantDecoder :: DecodeJson TS where
  decodeJson json = do
    nano <- decodeJson json |> map (dropWhile (_ /= '.')) |> map (drop 1) |> map (readInt 10)
    secs <- decodeJson json |> map (takeWhile (_ /= '.')) |> map (readInt 10)
    pure (TS { secs: secs, nanos: nano })

instance instantEncoder :: EncodeJson TS where
  encodeJson (TS record) = encodeJson ((show record.secs) <> (show record.nanos))
