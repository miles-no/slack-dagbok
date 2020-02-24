module Persistence where

import Data.Argonaut (decodeJson, encodeJson)
import Data.List (List)
import DataTypes (User, UserId(..), UserlogEntry)
import DateFormatting (Instant, toMillis)
import Effect.Promise (class Deferred, Promise)
import Firebase (CollectionReference, DocumentReference, QueryOperator(..), addDocument, collection, doc, query, rootCollection, runQueryParse, setDocument)
import Foreign (unsafeToForeign)
import Util ((|>))

usersRef :: CollectionReference
usersRef = rootCollection "users"

userRef :: UserId -> DocumentReference
userRef (UserId userId) = usersRef |> doc userId

updateUser :: User -> Promise DocumentReference
updateUser user = setDocument (encodeJson user) (userRef user.userId)

userLogRef :: UserId -> CollectionReference
userLogRef userId = userRef userId |> collection "log"

addUserlogEntry :: UserId -> UserlogEntry -> Promise String
addUserlogEntry userId userlogentry = do
  addDocument "userlogEntryId" (encodeJson userlogentry) (userLogRef userId)

loadUserlogEntries :: Deferred => UserId -> Instant -> Promise (List UserlogEntry)
loadUserlogEntries userId ins =
  query "posix" GreaterThan (unsafeToForeign (toMillis ins)) (userLogRef userId)
    |> runQueryParse decodeJson
