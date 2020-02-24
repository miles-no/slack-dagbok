exports.info = msg => obj => new Promise((res, rej) => {
    console.info(msg, obj)
    res()
})
exports.error = msg => obj => new Promise((res, rej) => {
    console.error(msg, obj)
    res()
})