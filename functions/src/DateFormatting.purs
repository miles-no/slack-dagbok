module DateFormatting where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Either.Nested (at1)
import Data.Int (floor, toNumber)
import Effect.Promise (class Deferred, Promise)
import Math (abs)
import Record (merge)
import Util ((|>))

type TimeZone
  = String

utc :: String
utc = "Etc/UTC"

europe_oslo :: TimeZone
europe_oslo = "Europe/Oslo"

newtype Year
  = Year Int

data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December

instance showMonth :: Show Month where
  show January = "January"
  show February = "February"
  show March = "March"
  show April = "April"
  show May = "May"
  show June = "June"
  show July = "July"
  show August = "August"
  show September = "September"
  show October = "October"
  show November = "November"
  show December = "December"

instance showYear :: Show Year where
  show (Year y) = show y

derive instance eqMonth :: Eq Month

monthNumber :: Month -> Int
monthNumber = case _ of
  January -> 1
  February -> 2
  March -> 3
  April -> 4
  May -> 5
  June -> 6
  July -> 7
  August -> 8
  September -> 9
  October -> 10
  November -> 11
  December -> 12

month :: Int -> Month
month =
  clamp 1 12
    >>> case _ of
        1 -> January
        2 -> February
        3 -> March
        4 -> April
        5 -> May
        6 -> June
        7 -> July
        8 -> August
        9 -> September
        10 -> October
        11 -> November
        _ -> December

-- | A type representing the days of the week in the Gregorian calendar.
data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

derive instance eqWeekday :: Eq Weekday

weekday :: Int -> Weekday
weekday =
  clamp 1 7
    >>> case _ of
        1 -> Monday
        2 -> Tuesday
        3 -> Wednesday
        4 -> Thursday
        5 -> Friday
        6 -> Saturday
        _ -> Sunday

isWorkday :: Weekday -> Boolean
isWorkday Saturday = false

isWorkday Sunday = false

isWorkday _ = true

instance showWeekday :: Show Weekday where
  show Monday = "Monday"
  show Tuesday = "Tuesday"
  show Wednesday = "Wednesday"
  show Thursday = "Thursday"
  show Friday = "Friday"
  show Saturday = "Saturday"
  show Sunday = "Sunday"

newtype Instant
  = Instant Int

instance instantEq :: Eq Instant where
  eq (Instant posixOne) (Instant posixOther) = posixOne == posixOther

instance instantOrd :: Ord Instant where
  compare (Instant posixOne) (Instant posixOther) = compare posixOne posixOther

newtype Duration
  = Duration Int

type DurationUnits
  = { standardDays :: Int
    , hours :: Int
    , minutes :: Int
    , seconds :: Int
    , milliseconds :: Int
    }

instance durationSemigroup :: Semigroup Duration where
  append (Duration millis) (Duration andmillis) = Duration (millis + andmillis)

newtype LocalDate
  = LocalDate { year :: Year, month :: Month, day :: Int }

newtype LocalTime
  = LocalTime { hour :: Int, minute :: Int, second :: Int, millisecond :: Int }

midnight :: LocalTime
midnight = LocalTime { hour: 0, minute: 0, second: 0, millisecond: 0 }

newtype LocalDateTime
  = LocalDateTime { localDate :: LocalDate, localTime :: LocalTime }

newtype ZonedDateTime
  = ZonedDateTime { localDate :: LocalDate, localTime :: LocalTime, zone :: TimeZone }

class Dated a where
  getDay :: a -> Int
  getMonth :: a -> Month
  getYear :: a -> Year
  atDay :: Int -> a -> a
  atMonth :: Month -> a -> a
  atYear :: Year -> a -> a

class LocalTimed a where
  getHour :: a -> Int
  getMinute :: a -> Int
  getSecond :: a -> Int
  getMillisecond :: a -> Int
  atHour :: Int -> a -> a
  atMinute :: Int -> a -> a
  atSecond :: Int -> a -> a
  atMillisecond :: Int -> a -> a

class ZoneAdjustable a where
  atZone :: TimeZone -> a -> ZonedDateTime

instance localDateDated :: Dated LocalDate where
  getDay (LocalDate ld) = ld.day
  getMonth (LocalDate ld) = ld.month
  getYear (LocalDate ld) = ld.year
  atDay d (LocalDate ld) = LocalDate (merge { day: d } ld)
  atMonth m (LocalDate ld) = LocalDate (merge { month: m } ld)
  atYear y (LocalDate ld) = LocalDate (merge { year: y } ld)

instance localDateTimeDated :: Dated LocalDateTime where
  getDay (LocalDateTime ld) = getDay ld.localDate
  getMonth (LocalDateTime ld) = getMonth ld.localDate
  getYear (LocalDateTime ld) = getYear ld.localDate
  atDay d (LocalDateTime ld) = LocalDateTime (merge { localDate: atDay d ld.localDate } ld)
  atMonth m (LocalDateTime ld) = LocalDateTime (merge { localDate: atMonth m ld.localDate } ld)
  atYear y (LocalDateTime ld) = LocalDateTime (merge { localDate: atYear y ld.localDate } ld)

instance zonedDateTimeDated :: Dated ZonedDateTime where
  getDay (ZonedDateTime r) = getDay r.localDate
  getMonth (ZonedDateTime ld) = getMonth ld.localDate
  getYear (ZonedDateTime ld) = getYear ld.localDate
  atDay d (ZonedDateTime ld) = ZonedDateTime (merge { localDate: atDay d ld.localDate } ld)
  atMonth m (ZonedDateTime ld) = ZonedDateTime (merge { localDate: atMonth m ld.localDate } ld)
  atYear y (ZonedDateTime ld) = ZonedDateTime (merge { localDate: atYear y ld.localDate } ld)

instance localTimeLocalTimed :: LocalTimed LocalTime where
  getHour (LocalTime lt) = lt.hour
  getMinute (LocalTime lt) = lt.minute
  getSecond (LocalTime lt) = lt.second
  getMillisecond (LocalTime lt) = lt.millisecond
  atHour hour (LocalTime lt) = LocalTime (merge { hour: clamp 0 23 hour } lt)
  atMinute minute (LocalTime lt) = LocalTime (merge { minute: clamp 0 59 minute } lt)
  atSecond second (LocalTime lt) = LocalTime (merge { hour: clamp 0 59 second } lt)
  atMillisecond millisceond (LocalTime lt) = LocalTime (merge { millisecond: clamp 0 999 millisceond } lt)

instance localDateTimeLocalTimed :: LocalTimed LocalDateTime where
  getHour (LocalDateTime ldt) = getHour ldt.localTime
  getMinute (LocalDateTime ldt) = getMinute ldt.localTime
  getSecond (LocalDateTime ldt) = getSecond ldt.localTime
  getMillisecond (LocalDateTime ldt) = getMillisecond ldt.localTime
  atHour hour (LocalDateTime ldt) = LocalDateTime (merge { localTime: atHour hour ldt.localTime } ldt)
  atMinute minute (LocalDateTime ldt) = LocalDateTime (merge { localTime: atMinute minute ldt.localTime } ldt)
  atSecond second (LocalDateTime ldt) = LocalDateTime (merge { localTime: atSecond second ldt.localTime } ldt)
  atMillisecond millisecond (LocalDateTime ldt) = LocalDateTime (merge { localTime: atMillisecond millisecond ldt.localTime } ldt)

instance zonedDateTimeLocalTimed :: LocalTimed ZonedDateTime where
  getHour (ZonedDateTime ldt) = getHour ldt.localTime
  getMinute (ZonedDateTime ldt) = getMinute ldt.localTime
  getSecond (ZonedDateTime ldt) = getSecond ldt.localTime
  getMillisecond (ZonedDateTime ldt) = getMillisecond ldt.localTime
  atHour hour (ZonedDateTime ldt) = ZonedDateTime (merge { localTime: atHour hour ldt.localTime } ldt)
  atMinute minute (ZonedDateTime ldt) = ZonedDateTime (merge { localTime: atMinute minute ldt.localTime } ldt)
  atSecond second (ZonedDateTime ldt) = ZonedDateTime (merge { localTime: atSecond second ldt.localTime } ldt)
  atMillisecond millisecond (ZonedDateTime ldt) = ZonedDateTime (merge { localTime: atMillisecond millisecond ldt.localTime } ldt)

instance ldtZoneAdjustable :: ZoneAdjustable LocalDateTime where
  atZone zone (LocalDateTime ldt) = ZonedDateTime (merge ldt { zone: zone })

instance zdtZoneAdjustable :: ZoneAdjustable ZonedDateTime where
  atZone zone (ZonedDateTime ldt) = ZonedDateTime (merge ldt { zone: zone })

year :: Int -> Year
year y = Year (clamp 0 3000 y)

getDate :: ZonedDateTime -> LocalDate
getDate (ZonedDateTime zdt) = zdt.localDate

getZone :: ZonedDateTime -> TimeZone
getZone (ZonedDateTime record) = record.zone

yearAsInt :: Year -> Int
yearAsInt (Year y) = y

localDate :: { year :: Int, month :: Int, day :: Int } -> LocalDate
localDate rec = LocalDate { year: year rec.year, month: month rec.month, day: rec.year }

localTime :: { hour :: Int, minute :: Int, second :: Int, millisecond :: Int } -> LocalTime
localTime fields = LocalTime fields

instantFromNumber :: Number -> Instant
instantFromNumber = abs >>> floor >>> Instant

instant :: Int -> Instant
instant n = if n < 0 then Instant (-n) else Instant n

toMillis :: Instant -> Int
toMillis (Instant millis) = millis

millisecondsIn :: Duration -> Int
millisecondsIn (Duration m) = m

asEpochOffset :: Instant -> Duration
asEpochOffset (Instant millis) = milliseconds millis

fromEpochOffset :: Duration -> Instant
fromEpochOffset (Duration duration) = Instant duration

milliseconds :: Int -> Duration
milliseconds m = Duration m

millisFromNumber :: Number -> Duration
millisFromNumber = floor >>> milliseconds

seconds :: Int -> Duration
seconds s = Duration (s * 1000)

secondsIn :: Duration -> Int
secondsIn (Duration d) = d / 1000

minutes :: Int -> Duration
minutes m = seconds (m * 60)

minutesIn :: Duration -> Int
minutesIn d = (secondsIn d) / 60

hours :: Int -> Duration
hours h = minutes (h * 60)

hoursIn :: Duration -> Int
hoursIn d = (minutesIn d) / 60

standardDays :: Int -> Duration
standardDays d = hours (d * 24)

standardDaysIn :: Duration -> Int
standardDaysIn d = (hoursIn d) / 24

negateDuration :: Duration -> Duration
negateDuration (Duration d) = Duration (-d)

unitsIn :: Duration -> DurationUnits
unitsIn d = { standardDays: days, hours: hrs, minutes: mins, seconds: secs, milliseconds: mills }
  where
  days = standardDaysIn d

  remainingHours = d <> (negateDuration (standardDays days))

  hrs = hoursIn remainingHours

  remainingMinutes = remainingHours <> (negateDuration (hours hrs))

  mins = minutesIn remainingMinutes

  remainingSecs = remainingMinutes <> (negateDuration (minutes mins))

  secs = secondsIn remainingSecs

  remainingMillis = remainingSecs <> (negateDuration (seconds secs))

  mills = millisecondsIn remainingMillis

isLeapYear :: Year -> Boolean
isLeapYear (Year y') = (mod y' 4 == 0) && ((mod y' 400 == 0) || not (mod y' 100 == 0))

daysInMonth :: Year -> Month -> Int
daysInMonth y m = case m of
  January -> 31
  February
    | isLeapYear y -> 29
    | otherwise -> 28
  March -> 31
  April -> 30
  May -> 31
  June -> 30
  July -> 31
  August -> 31
  September -> 30
  October -> 31
  November -> 30
  December -> 31

getWeekday :: ZonedDateTime -> Weekday
getWeekday (ZonedDateTime zdt) = weekday (floor weekdaynum)
  where
  record = { year: yearAsInt (getYear zdt.localDate), month: monthNumber (getMonth zdt.localDate), day: (getDay zdt.localDate) }

  lt = case zdt.localTime of
    LocalTime l -> l

  weekdaynum = weekdayImpl record lt zdt.zone

getWeeknumber :: ZonedDateTime -> Int
getWeeknumber (ZonedDateTime zdt) = floor weeknumber
  where
  record = { year: yearAsInt (getYear zdt.localDate), month: monthNumber (getMonth zdt.localDate), day: (getDay zdt.localDate) }

  lt = case zdt.localTime of
    LocalTime l -> l

  weeknumber = weeknumberImpl record lt zdt.zone

findNextDayAfter :: (ZonedDateTime -> Boolean) -> ZonedDateTime -> ZonedDateTime
findNextDayAfter pred zdt =
  let
    nextDay = toInstant zdt |> appendDuration (standardDays 1) |> toZonedDateTime (getZone zdt)
  in
    if pred nextDay then nextDay else findNextDayAfter pred nextDay

toZonedDateTime :: TimeZone -> Instant -> ZonedDateTime
toZonedDateTime tz (Instant i) = toDateTimeImpl construct n tz
  where
  n = toNumber i

  construct :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> ZonedDateTime
  construct y m d hour min sec milli = ZonedDateTime { localDate: LocalDate { year: year y, month: month m, day: d }, localTime: localTime { hour: hour, minute: min, second: sec, millisecond: milli }, zone: tz }

toInstant :: ZonedDateTime -> Instant
toInstant (ZonedDateTime zdt) = instantFromNumber n
  where
  record = { year: yearAsInt (getYear zdt.localDate), month: monthNumber (getMonth zdt.localDate), day: (getDay zdt.localDate) }

  lt = case zdt.localTime of
    LocalTime l -> l

  n = toInstantImpl record lt zdt.zone

appendDuration :: Duration -> Instant -> Instant
appendDuration (Duration duration) (Instant origin) = Instant (duration + origin)

isBefore :: Instant -> Instant -> Boolean
isBefore at time = time < at

isAfter :: Instant -> Instant -> Boolean
isAfter at time = time > at

now :: Deferred => Unit -> Promise Instant
now _ = nowImpl unit |> map floor |> map Instant

instance instantDecoder :: DecodeJson Instant where
  decodeJson json = decodeJson json |> map Instant

instance instantEncoder :: EncodeJson Instant where
  encodeJson (Instant i) = encodeJson i

foreign import toDateTimeImpl ::
  (Int -> Int -> Int -> Int -> Int -> Int -> Int -> ZonedDateTime) ->
  Number ->
  TimeZone ->
  ZonedDateTime

foreign import weekdayImpl :: { year :: Int, month :: Int, day :: Int } -> { hour :: Int, minute :: Int, second :: Int, millisecond :: Int } -> TimeZone -> Number

foreign import weeknumberImpl :: { year :: Int, month :: Int, day :: Int } -> { hour :: Int, minute :: Int, second :: Int, millisecond :: Int } -> TimeZone -> Number

foreign import toInstantImpl :: { year :: Int, month :: Int, day :: Int } -> { hour :: Int, minute :: Int, second :: Int, millisecond :: Int } -> TimeZone -> Number

foreign import nowImpl :: Unit -> Promise Number
