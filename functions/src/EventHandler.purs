module EventHandler where

import Effect.Promise
import Console (info)
import Data.Argonaut (encodeJson)
import Data.List (List(..), (:), filter)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import DataTypes (Action(..), Message(..), Trigger, TriggerSchedule(..), TriggerState, TriggerWithState, User, UserId(..), nextTriggerInstant, tsToInstant)
import DateFormatting (Instant, atEpoch, isAfter, localTime)
import Persistence (addUserlogEntry, deleteUserLogEntries, findUser, loadTriggerStates, loadUser, loadActiveUsers, saveTrigger, updateUser)
import Prelude (Unit, bind, map, pure, unit, (<>))
import Record (merge, delete)
import Slack (postMessage, userInfo, viewPublish)
import Util ((|>), joinPromises)

handleEvent :: Deferred => Message -> Promise Unit
handleEvent NoOp = pure unit

handleEvent (ChatMessage record) = do
  _ <- addUserlogEntry record.userId { userId: record.userId, text: record.text, posix: tsToInstant record.ts }
  _ <- viewPublish record.userId
  pure unit

handleEvent (AppHomeOpened record) = do
  user <- userInfo record.user
  maybeUser <- findUser (UserId user.userId)
  _ <- updateUser { userId: record.user, name: user.name, channel: record.channel, active: maybeUser |> map (\u -> u.active) |> fromMaybe true }
  pure unit

handleEvent (Tick now) = do
  states <- triggerStatesAsMap
  executingTriggers <- pure ((triggers |> map (stateOfTrigger states)) |> filter (shallExeute now.posix))
  users <- loadActiveUsers
  (executingTriggers |> map (fireTrigger now.posix users)) <> (executingTriggers |> map (nextState now.posix) |> map saveStateOfTrigger)
    |> sequence
    |> map (\_ -> unit)

handleEvent (ActionMessage record) = do
  info "ActionMessage" (encodeJson record)

-- ACTIONS
handleAction :: Deferred => Action -> Promise Unit
handleAction (Start record) = do
  user <- loadUser (UserId record.userId)
  updateUser (merge { active: true } user) |> map (\_ -> unit)

handleAction (Stop record) = do
  user <- loadUser (UserId record.userId)
  updateUser (merge { active: false } user) |> map (\_ -> unit)

handleAction (Clear record) = do
  user <- loadUser (UserId record.userId)
  deleteUserLogEntries user.userId

-- TRIGGERS
shallExeute :: Instant -> TriggerWithState -> Boolean
shallExeute now trigger = isAfter trigger.nextInstant now

triggers :: List Trigger
triggers =
  { name: "morning_greeting", schedule: (EveryWorkdayAt (localTime { hour: 8, minute: 5, second: 0, millisecond: 0 })) }
    : { name: "afternoon_reminder", schedule: (EveryWorkdayAt (localTime { hour: 16, minute: 2, second: 40, millisecond: 0 })) }
    : Nil

stateOfTrigger :: (Map String TriggerState) -> Trigger -> TriggerWithState
stateOfTrigger states trigger =
  lookup trigger.name states
    |> map (\s -> merge { nextInstant: s.nextInstant } trigger)
    |> fromMaybe (merge { nextInstant: atEpoch } trigger)

saveStateOfTrigger :: Deferred => TriggerWithState -> Promise Unit
saveStateOfTrigger trigger = info "Saving" (encodeJson state) <> saveTrigger state
  where
  state = delete (SProxy :: SProxy "schedule") trigger

nextState :: Instant -> TriggerWithState -> TriggerWithState
nextState now trigger =
  let
    nextExec = nextTriggerInstant now trigger.schedule
  in
    merge { nextInstant: nextExec } trigger

triggerStatesAsMap :: Deferred => Promise (Map String TriggerState)
triggerStatesAsMap = loadTriggerStates |> map (\list -> list |> map (\ts -> Tuple ts.name ts) |> fromFoldable)

fireTrigger :: Deferred => Instant -> List User -> TriggerWithState -> Promise Unit
fireTrigger at users trigger = joinPromises (users |> map (fireTriggerForUser at trigger))

fireTriggerForUser :: Deferred => Instant -> TriggerWithState -> User -> Promise Unit
fireTriggerForUser at trigger user = case trigger.name of
  "morning_greeting" -> postMessage { channel: user.channel, text: "God morgen " <> user.name <> ", hva tenker du å gjøre i dag?" }
  "afternoon_reminder" -> postMessage { channel: user.channel, text: "God ettermiddag " <> user.name <> ", hva gjorde du i dag da?" }
  _ -> pure unit
