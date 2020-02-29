module Slack where

import Prelude
import Console as Console
import Data.Argonaut (encodeJson)
import Data.Array (cons, foldl)
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..), fst, snd)
import DataTypes (UserId(..), UserlogEntry)
import DateFormatting (TimeZone, getDay, getHour, getMinute, getMonth, instant, toZonedDateTime)
import Effect.Promise (class Deferred, Promise)
import Persistence (loadUserlogEntries)
import Util (leftpad)

type SlackMessage
  = { channel :: String
    , text :: String
    }

type SlackUser
  = { name :: String
    , userId :: String
    }

type SlackUserWeek
  = { date :: String
    , entries :: Array SlackUserlogEntry
    }

type SlackUserlogEntry
  = { time :: String
    , text :: String
    }

foreign import postMessage :: SlackMessage -> Promise Unit

foreign import doUserInfo :: String -> Promise SlackUser

userInfo :: UserId -> Promise SlackUser
userInfo (UserId userId) = doUserInfo userId

foreign import doViewPublish :: String -> Array SlackUserWeek -> Promise Unit

viewPublish :: Deferred => UserId -> Promise Unit
viewPublish (UserId userId) = do
  entries <- loadUserlogEntries (UserId userId) (instant 0)
  _ <- Console.info "Entries" (encodeJson entries)
  _ <- doViewPublish userId (groupEntries entries)
  pure unit

groupEntries :: List UserlogEntry -> Array SlackUserWeek
groupEntries Nil = []

groupEntries (head : tail) = cons (fst t) (snd t)
  where
  t = (foldl accum (Tuple (newSlackUserWeek head) []) tail)

  accum :: Tuple SlackUserWeek (Array SlackUserWeek) -> UserlogEntry -> Tuple SlackUserWeek (Array SlackUserWeek)
  accum (Tuple current arr) entry =
    if (not (nextdatestr == current.date)) then
      Tuple (newSlackUserWeek entry) (cons current arr)
    else
      Tuple (appendEntry entry current) arr
    where
    nextdatestr = entryDate entry

appendEntry :: UserlogEntry -> SlackUserWeek -> SlackUserWeek
appendEntry entry suw = { date: suw.date, entries: cons { time: entryTime entry, text: entry.text } suw.entries }

newSlackUserWeek :: UserlogEntry -> SlackUserWeek
newSlackUserWeek entry = { date: nextdatestr, entries: [ { time: entryTime entry, text: entry.text } ] }
  where
  nextdatestr = entryDate entry

europe_oslo :: TimeZone
europe_oslo = "Europe/Oslo"

entryDate :: UserlogEntry -> String
entryDate entry = datestr
  where
  zdt = toZonedDateTime entry.posix europe_oslo

  datestr = (leftpad 2 "0" (show (getDay zdt))) <> "/" <> (leftpad 2 "0" (show (getMonth zdt)))

entryTime :: UserlogEntry -> String
entryTime entry = timestr
  where
  zdt = toZonedDateTime entry.posix europe_oslo

  timestr = (leftpad 2 "0" (show (getHour zdt))) <> ":" <> (leftpad 2 "0" (show (getMinute zdt)))