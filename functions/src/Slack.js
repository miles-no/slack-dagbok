const functions = require('firebase-functions');
const { WebClient } = require('@slack/web-api');
const web = new WebClient(functions.config().slack.token);
const { DateTime } = require("luxon");

exports.postMessage = message =>
   web.chat.postMessage(message)


exports.doUserInfo = async userId => {
   const u = await web.users.info({ user: userId })
   return {
      name: u.user.profile.real_name,
      userId: userId
   }
}

exports.doViewPublish = userId => entries => {

   console.log("Updating home ", entries)
   const sections = entries.map(logentry => (
      {
         "type": "section",
         "text": {
            "type": "mrkdwn",
            "text": "*" + logentry.date + "*\n" + (logentry.entries.map(e => e.time + " - " + e.text).join("\n"))
         }

      }))
   return web.views.publish({
      user_id: userId,
      view: {
         "type": "home",
         "blocks": sections.concat([
            {
               "type": "actions",
               "elements": [
                  {
                     "type": "button",
                     "text": {
                        "type": "plain_text",
                        "text": "Action A",
                        "emoji": true
                     },
                     value: "b"
                  },
                  {
                     "type": "button",
                     "text": {
                        "type": "plain_text",
                        "text": "Action B",
                        "emoji": true
                     },
                     value: "c"
                  }
               ]
            }
         ])
      }
   })
}
