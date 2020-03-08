module NextDateTimeSpec where

import Prelude
import DataTypes (TriggerSchedule(..), nextTriggerInstant)
import DateFormatting (Instant, durationBetween, europe_oslo, getHour, getMillisecond, getMinute, getSecond, getWeekday, hoursIn, instant, localTime, standardDays, toZonedDateTime)
import Test.QuickCheck (class Arbitrary, (<?>), (===))
import Test.QuickCheck.Gen (choose)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Util ((|>))

time :: { hour :: Int, minute :: Int, second :: Int, millisecond :: Int }
time = { hour: 8, minute: 15, second: 8, millisecond: 2 }

sched :: TriggerSchedule
sched = EveryWorkdayAt (localTime time)

newtype RandInstant
  = RandInstant Instant

instance randInstant :: Arbitrary RandInstant where
  arbitrary = choose 0.0 1583611552000.0 |> map (instant >>> RandInstant)

spec :: Spec Unit
spec =
  describe "Math" do
    it "is always on the scheduled time"
      $ quickCheck \(RandInstant instant) ->
          let
            next = nextTriggerInstant instant sched

            dt = toZonedDateTime europe_oslo next
          in
            { hour: getHour dt, minute: getMinute dt, second: getSecond dt, millisecond: getMillisecond dt } === time
    it "is always after"
      $ quickCheck \(RandInstant instant) ->
          let
            next = nextTriggerInstant instant sched
          in
            (next > instant) == true <?> "The next instant must be after the current"
    it "is always within three days"
      $ quickCheck \(RandInstant instant) ->
          let
            next = nextTriggerInstant instant sched

            nowDT = toZonedDateTime europe_oslo instant

            nextDt = toZonedDateTime europe_oslo next

            distance = durationBetween instant next

            maxDistance = standardDays 4
          in
            (distance < maxDistance) == true <?> "The next instant must never be more than " <> show (hoursIn maxDistance) <> " later ,but is" <> show (hoursIn distance) <> "hours " <> show (getWeekday nowDT) <> " - " <> show (getWeekday nextDt)
