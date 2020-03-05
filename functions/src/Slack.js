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

exports.doOpenSheetInModal = userId => {
   return web.views.open({
      type: 'modal',
      title: {
         type: 'plain_text',
         text: 'Create a stickie note'
      },
      submit: {
         type: 'plain_text',
         text: 'Create',
         value: "c"
      },
      blocks: [
         {
            "type": "section",
            "text": {
               "type": "plain_text",
               "text": "Enter the id of sheet (you can find it in the url of the sheet)"
            }
         },

         {
            "type": "input",
            "block_id": "sheet_id_modal",
            "label": {
               "type": "plain_text",
               "text": "Sheet ID"
            },
            "element": {
               "action_id": "content",
               "type": "plain_text_input",
               "placeholder": {
                  "type": "plain_text",
                  "text": "Take a note... \n(Text longer than 3000 characters will be truncated!)"
               },
               "multiline": false
            }
         },
      ]
   })
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
