const functions = require('firebase-functions');
const admin = require('firebase-admin');

const { WebClient } = require('@slack/web-api');
const { createEventAdapter } = require('@slack/events-api');
const { createMessageAdapter } = require('@slack/interactive-messages');



admin.initializeApp();

const web = new WebClient(functions.config().slack.token);

const slackSigningSecret = functions.config().slack.signingsecret;
const slackEvents = createEventAdapter(slackSigningSecret);
const slackInteractions = createMessageAdapter(slackSigningSecret);

const listener = slackEvents.requestListener()
const interactionListener = slackInteractions.requestListener()
var storeduser = { name: "", channelId: "" }
const index_js_1 = require("./output/Main/index.js");

slackEvents.on('error', (error) => {
    console.error(error.name, error); // TypeError
    const answer = index_js_1.handleIncoming()("error")(error);

    return answer;
});

slackEvents.on('message', async (msg) => {
    await index_js_1.handleIncoming()("message")(msg);

})

slackEvents.on('app_home_opened', async (event) => {
    await index_js_1.handleIncoming()("app_home_opened")(event);
})


slackInteractions.action({ type: 'button' }, (payload, respond) => {
    console.log(payload)
})



exports.onMessage = functions.https.onRequest(async (request, response) => {

    if (request.body.challenge)
        response.send(request.body.challenge)
    else if (request.body.payload) {
        interactionListener(request, response)
    }
    else {
        await index_js_1.handleIncoming()("tick")({
            tick: Date.now()
        });
        listener(request, response)
    }


});



exports.scheduledFunction = functions.pubsub.schedule('every 5 minutes').onRun((context) => {

    index_js_1.handleIncoming()("tick")({
        tick: Date.now()
    });
    return null;
});

/*
//Sheets integration

const { OAuth2Client } = require('google-auth-library');
const { google } = require('googleapis');

const CONFIG_CLIENT_ID = functions.config().googleapi.client_id;
const CONFIG_CLIENT_SECRET = functions.config().googleapi.client_secret;
const CONFIG_SHEET_ID = functions.config().googleapi.sheet_id;

// The OAuth Callback Redirect.
const FUNCTIONS_REDIRECT = `https://${process.env.FUNCTION_REGION}-${process.env.GCP_PROJECT}.cloudfunctions.net/oauthcallback`;

// setup for authGoogleAPI
const SCOPES = ['https://www.googleapis.com/auth/spreadsheets'];
const functionsOauthClient = new OAuth2Client(CONFIG_CLIENT_ID, CONFIG_CLIENT_SECRET, FUNCTIONS_REDIRECT);

// OAuth token cached locally.
let oauthTokens = null;

// visit the URL for this Function to request tokens
exports.authgoogleapi = functions.https.onRequest((req, res) => {
    res.set('Cache-Control', 'private, max-age=0, s-maxage=0');
    res.redirect(functionsOauthClient.generateAuthUrl({
        access_type: 'offline',
        scope: SCOPES,
        prompt: 'consent',
    }));
});

// setup for OauthCallback
const DB_TOKEN_PATH = '/api_tokens';

// after you grant access, you will be redirected to the URL for this Function
// this Function stores the tokens to your Firebase database
exports.oauthcallback = functions.https.onRequest(async (req, res) => {
    res.set('Cache-Control', 'private, max-age=0, s-maxage=0');
    const code = req.query.code;
    try {
        const { tokens } = await functionsOauthClient.getToken(code);
        // Now tokens contains an access_token and an optional refresh_token. Save them.
        await admin.database().ref(DB_TOKEN_PATH).set(tokens);
        return res.status(200).send('App successfully configured with new Credentials. '
            + 'You can now close this page.');
    } catch (error) {
        return res.status(400).send(error);
    }
});

// trigger function to write to Sheet when new data comes in on CONFIG_DATA_PATH
exports.appendrecordtospreadsheet = functions.https.onRequest(async (req, res) => {

    return appendPromise({
        spreadsheetId: CONFIG_SHEET_ID,
        range: 'A:C',
        valueInputOption: 'USER_ENTERED',
        insertDataOption: 'INSERT_ROWS',
        resource: {
            values: [["A", "B", "C"]],
        },
    });
});

// accepts an append request, returns a Promise to append it, enriching it with auth
function appendPromise(requestWithoutAuth) {
    return new Promise((resolve, reject) => {
        return getAuthorizedClient().then((client) => {
            const sheets = google.sheets('v4');
            const request = requestWithoutAuth;
            request.auth = client;
            return sheets.spreadsheets.values.append(request, (err, response) => {
                if (err) {
                    console.error(`The API returned an error: ${err}`);
                    return reject(err);
                }
                return resolve(response.data);
            });
        });
    });
}

// checks if oauthTokens have been loaded into memory, and if not, retrieves them
async function getAuthorizedClient() {
    if (oauthTokens) {
        return functionsOauthClient;
    }
    const snapshot = await admin.database().ref(DB_TOKEN_PATH).once('value');
    oauthTokens = snapshot.val();
    functionsOauthClient.setCredentials(oauthTokens);
    return functionsOauthClient;
}


*/