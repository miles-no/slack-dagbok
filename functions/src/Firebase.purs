module Firebase where

import Data.Argonaut

import Control.Bind (bindFlipped)
import Data.Array as Array
import Data.Either (Either, either, note)
import Data.List (List, fold, fromFoldable, mapMaybe, null)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect.Exception (error)
import Effect.Promise (class Deferred, Promise, reject)
import Foreign (Foreign)
import Prelude (Unit, map, pure, (<>), (>>=))
import Util ((|>))

-- <, <=, ==, >, >=, or array-contains
data QueryOperator
  = LessThan
  | GreaterThan
  | Equal
  | LessThanOrEqual
  | GreaterThanOrEqual
  | ArrayContains

operatorAsString :: QueryOperator -> String
operatorAsString LessThan = "<"

operatorAsString GreaterThan = ">"

operatorAsString Equal = "=="

operatorAsString LessThanOrEqual = "<="

operatorAsString GreaterThanOrEqual = ">="

operatorAsString ArrayContains = "array-contains"

foreign import data DocumentReference :: Type

foreign import data CollectionReference :: Type

foreign import data Query :: Type

foreign import pathFromRef :: DocumentReference -> String

foreign import collTostring :: CollectionReference -> String

foreign import docTostring :: DocumentReference -> String

foreign import rootDoc :: String -> DocumentReference

foreign import rootCollection :: String -> CollectionReference

foreign import collection :: String -> DocumentReference -> CollectionReference

foreign import doc :: String -> CollectionReference -> DocumentReference

foreign import doGet :: (Maybe Json) -> (Json -> Maybe Json) -> DocumentReference -> Promise (Maybe Json)

foreign import findAll :: CollectionReference -> Promise (Array Json)

foreign import findAllDocuments :: CollectionReference -> Promise (Array DocumentReference)

findAllDocumentParse :: Deferred => forall a. (Json -> Either String a) -> CollectionReference -> Promise (List a)
findAllDocumentParse parser ref =
  findAll ref
    |> map fromFoldable
    |> map (\list -> list |> map parser)
    |> bindFlipped
        ( \l ->
            let
              errors = l |> mapMaybe (either Just (\_ -> Nothing))

              oks = l |> mapMaybe (either (\_ -> Nothing) Just)
            in
              if (null errors) then
                pure oks
              else
                reject (error (fold errors))
        )

foreign import doWhere :: String -> String -> Foreign -> CollectionReference -> Query

foreign import doAndWhere :: String -> String -> String -> Query -> Query

foreign import doFind :: Query -> Promise (Array Json)

findDocument :: DocumentReference -> Promise (Maybe Json)
findDocument ref = doGet Nothing Just ref

findDocumentParse :: Deferred => forall a. (Json -> Either String a) -> DocumentReference -> Promise (Maybe a)
findDocumentParse parser ref =
  findDocument ref
    |> bindFlipped
        ( case _ of
            Just json -> (parser json) |> either (\msg -> reject (error msg)) (\a -> pure (Just a))
            _ -> pure Nothing
        )

onFound :: Deferred => forall a b. (a -> Promise b) -> Promise (Maybe a) -> Promise (Maybe b)
onFound f aPromise =
  aPromise
    |> bindFlipped
        ( \maybeA -> case maybeA of
            Nothing -> pure Nothing
            Just a -> f a |> map Just
        )

onEach :: Deferred => forall a b. (a -> Promise b) -> Promise (Array a) -> Promise (Array b)
onEach f aPromise =
  aPromise
    |> bindFlipped
        (\arrA -> sequence (arrA |> map f))

getDocumentParse :: Deferred => forall a. (Json -> Either String a) -> DocumentReference -> Promise a
getDocumentParse parser ref =
  findDocument ref
    |> map (note ("Document at " <> (pathFromRef ref) <> " does not exist "))
    |> map (\eitherNotFoundOrJson -> eitherNotFoundOrJson >>= parser)
    >>= (\eitherErrOrValue -> either (\msg -> reject (error ("Failed reading document at " <> (pathFromRef ref) <> ": " <> msg))) pure eitherErrOrValue)

query :: String -> QueryOperator -> Foreign -> CollectionReference -> Query
query field operator value collectionReference = doWhere field (operatorAsString operator) value collectionReference

andQuery :: String -> QueryOperator -> String -> Query -> Query
andQuery field operator value q = doAndWhere field (operatorAsString operator) value q

runQuery :: Query -> Promise (Array Json)
runQuery q = doFind q

runQueryParse :: Deferred => forall a. (Json -> Either String a) -> Query -> Promise (List a)
runQueryParse parser q =
  doFind q
    |> map fromFoldable
    |> map (\list -> list |> map parser)
    |> bindFlipped
        ( \l ->
            let
              errors = l |> mapMaybe (either Just (\_ -> Nothing))

              oks = l |> mapMaybe (either (\_ -> Nothing) Just)
            in
              if (null errors) then
                pure oks
              else
                reject (error (fold errors))
        )

foreign import collectionGroup :: String -> Query

foreign import addDocument :: String -> Json -> CollectionReference -> Promise String

foreign import deleteDocument :: DocumentReference -> Promise Unit

foreign import setDocument :: Json -> DocumentReference -> Promise DocumentReference

foreign import mergeDocument :: Json -> DocumentReference -> Promise DocumentReference

foreign import addToArray :: String -> Json -> DocumentReference -> Promise DocumentReference

foreign import removeFromArray :: String -> Json -> DocumentReference -> Promise DocumentReference

foreign import doUpdateField :: Array String -> Json -> DocumentReference -> Promise Unit

updateField :: List String -> Json -> DocumentReference -> Promise Unit
updateField path value documentReference = doUpdateField (Array.fromFoldable path) value documentReference

foreign import doDeleteField :: Array String -> DocumentReference -> Promise Unit

deleteField :: List String -> DocumentReference -> Promise Unit
deleteField path documentReference = doDeleteField (Array.fromFoldable path) documentReference
