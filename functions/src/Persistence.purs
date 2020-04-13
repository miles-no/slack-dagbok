module Persistence where

import Prelude

import Data.Argonaut (decodeJson, encodeJson)
import Data.List (List)
import Data.Maybe (Maybe)
import DataTypes (TriggerState, User, UserId(..), UserlogEntry, userDecoder)
import DateFormatting (Instant, toMillis)
import Effect.Promise (class Deferred, Promise)
import Firebase (CollectionReference, DocumentReference, QueryOperator(..), addDocument, collection, deleteCollection, doc, findAllDocumentParse, findDocumentParse, getDocumentParse, query, rootCollection, runQueryParse, setDocument)
import Foreign (unsafeToForeign)
import Util ((|>))

usersRef :: CollectionReference
usersRef = rootCollection "users"

userRef :: UserId -> DocumentReference
userRef (UserId userId) = usersRef |> doc userId

updateUser :: User -> Promise DocumentReference
updateUser user = setDocument (encodeJson user) (userRef user.userId)

loadUsers :: Deferred => Promise (List User)
loadUsers = findAllDocumentParse userDecoder usersRef

loadUser :: Deferred => UserId -> Promise ( User)
loadUser userId = getDocumentParse userDecoder (userRef userId)

findUser :: Deferred => UserId -> Promise (Maybe User)
findUser userId = findDocumentParse userDecoder (userRef userId)



userLogRef :: UserId -> CollectionReference
userLogRef userId = userRef userId |> collection "log"

addUserlogEntry :: UserId -> UserlogEntry -> Promise String
addUserlogEntry userId userlogentry = do
  addDocument "userlogEntryId" (encodeJson userlogentry) (userLogRef userId)

loadUserlogEntries :: Deferred => UserId -> Instant -> Promise (List UserlogEntry)
loadUserlogEntries userId ins =
  query "posix" GreaterThan (unsafeToForeign (toMillis ins)) (userLogRef userId)
    |> runQueryParse decodeJson

deleteUserLogEntries :: Deferred => UserId -> Promise Unit
deleteUserLogEntries  userId  =
  deleteCollection (userLogRef  userId)

triggerStatesRef :: CollectionReference
triggerStatesRef = rootCollection "triggers"

triggerStateRef :: String -> DocumentReference
triggerStateRef id = triggerStatesRef |> doc id

loadTriggerStates :: Deferred => Promise (List TriggerState)
loadTriggerStates = findAllDocumentParse decodeJson triggerStatesRef

saveTrigger :: Deferred => TriggerState -> Promise Unit
saveTrigger trigger = setDocument (encodeJson trigger) (triggerStateRef trigger.name) |> map (\_ -> unit)
