const { DateTime } = require("luxon");





exports.nowImpl = () => new Promise((res, rej) => res(Date.now()))


exports.toDateTimeImpl = constructor => instant => timezone => {
  console.log("Input to Luxor", instant)
  console.log("Input to Luxor", timezone)
  const m = DateTime.fromMillis(instant).setZone(timezone)
  return constructor(m.year)(m.month)(m.day)(m.hour)(m.minute)(m.second)(m.millisecond)
}

exports.weekdayImpl = localdate => localtime => timezone =>
  DateTime.local(localdate.year, localdate.month, localdate.day, localtime.hour, localtime.minute, localtime.second, localtime.millisecond)
    .setZone(timezone).weekday

exports.weeknumberImpl = localdate => localtime => timezone =>
  DateTime.local(localdate.year, localdate.month, localdate.day, localtime.hour, localtime.minute, localtime.second, localtime.millisecond)
    .setZone(timezone).weekNumber

exports.toInstantImpl = localdate => localtime => timezone =>
  DateTime.local(localdate.year, localdate.month, localdate.day, localtime.hour, localtime.minute, localtime.second, localtime.millisecond)
    .setZone(timezone).toMillis
