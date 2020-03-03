module EventHandler where

import Effect.Promise

import Console (info)
import Data.Argonaut (encodeJson)
import Data.List (List(..), (:), filter)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe ( fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import DataTypes (Message(..), Trigger, TriggerSchedule(..), TriggerState, TriggerWithState, nextTriggerInstant, tsToInstant)
import DateFormatting (Instant, instant, isAfter, localTime)
import Persistence (addUserlogEntry, loadTriggerStates, saveTrigger, updateUser)
import Prelude (Unit, bind, map, pure, unit, (<>))
import Record (merge, delete)
import Slack (userInfo, viewPublish)
import Util ((|>))

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

handleEvent (Tick now) = do
  states <- triggerStatesAsMap
  executingTriggers <- pure ((triggers |> map (stateOfTrigger states)) |> filter (shallExeute now.posix))
  (executingTriggers |> map (fireTrigger now.posix)) <> (executingTriggers |> map saveStateOfTrigger)
    |> sequence
    |> map (\_ -> unit)

shallExeute :: Instant -> TriggerWithState -> Boolean
shallExeute now trigger = isAfter trigger.nextInstant now

triggers :: List Trigger
triggers =
  { name: "morning_greeting", schedule: (EveryWeekdayAt (localTime { hour: 8, minute: 15, second: 0, millisecond: 0 })) }
    : { name: "afternoon_reminder", schedule: (EveryWeekdayAt (localTime { hour: 15, minute: 50, second: 0, millisecond: 0 })) }
    : Nil

stateOfTrigger :: (Map String TriggerState) -> Trigger -> TriggerWithState
stateOfTrigger states trigger =
  lookup trigger.name states
    |> map (\s -> merge { nextInstant: s.nextInstant } trigger)
    |> fromMaybe (merge { nextInstant: instant 0 } trigger)


saveStateOfTrigger :: Deferred => TriggerWithState -> Promise Unit
saveStateOfTrigger trigger = 
  saveTrigger (delete (SProxy :: SProxy "schedule") trigger)

nextState :: Instant -> Trigger -> TriggerState
nextState now trigger =
  let
    nextExec = nextTriggerInstant now trigger.schedule
  in
    { nextInstant: nextExec
    , name: trigger.name
    }

triggerStatesAsMap :: Deferred => Promise (Map String TriggerState)
triggerStatesAsMap = loadTriggerStates |> map (\list -> list |> map (\ts -> Tuple ts.name ts) |> fromFoldable)

fireTrigger :: Deferred => Instant -> TriggerWithState -> Promise Unit
fireTrigger at trigger = pure unit
