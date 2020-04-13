const { DateTime } = require("luxon");





exports.nowImpl = () => new Promise((res, rej) => res(Date.now()))


exports.toDateTimeImpl = constructor => instant => timezone => {
  const m = DateTime.fromMillis(instant).setZone(timezone)
  return constructor(m.year)(m.month)(m.day)(m.hour)(m.minute)(m.second)(m.millisecond)
}

exports.weekdayImpl = localdate => localtime => timezone =>
  DateTime.fromObject(
    {
      year: localdate.year,
      month: localdate.month,
      day: localdate.day,
      hour: localtime.hour,
      minute: localtime.minute,
      second: localtime.second,
      millisecond: localtime.millisecond,
      zone: timezone
    }).weekday

exports.weeknumberImpl = localdate => localtime => timezone =>
  DateTime.fromObject(
    {
      year: localdate.year,
      month: localdate.month,
      day: localdate.day,
      hour: localtime.hour,
      minute: localtime.minute,
      second: localtime.second,
      millisecond: localtime.millisecond,
      zone: timezone
    }).weekNumber

exports.toInstantImpl = localdate => localtime => timezone => {
  const placed = DateTime.fromObject(
    {
      year: localdate.year,
      month: localdate.month,
      day: localdate.day,
      hour: localtime.hour,
      minute: localtime.minute,
      second: localtime.second,
      millisecond: localtime.millisecond,
      zone: timezone
    })
  return placed.toMillis()
}