module Util where

import Data.Argonaut (class DecodeJson, Json, decodeJson, (.:))
import Data.Either (Either(..))
import Data.List (List(..), findIndex, foldl, reverse, (:))
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String (length)
import Data.Traversable (class Traversable, sequence)
import Effect.Promise (class Deferred, Promise)
import Prelude (class Applicative, class Apply, class Bind, class Eq, class Functor, Unit, eq, map, otherwise, unit, (<*>), (<>), (>=), (>>=))

type DecodeFunction a
  = Json -> Either String a

newtype Decoder a
  = Decoder (Json -> Either String a)

instance decoderFunctor :: Functor Decoder where
  map f (Decoder fa) = Decoder g
    where
    g value = map f (fa value)

instance decoderApply :: Apply Decoder where
  apply (Decoder fFab) (Decoder fa) = Decoder g
    where
    g value = (fa value) >>= (\a -> map (\fab -> fab a) (fFab value))

instance decoderApplicative :: Applicative Decoder where
  pure p = Decoder g
    where
    g _ = Right p

instance bindDecoder :: Bind Decoder where
  bind (Decoder af) f = Decoder g
    where
    g value =
      let
        fa = af value

        fb = fa >>= (\a -> decode (f a) value)
      in
        fb

required :: forall a b. DecodeJson a => String -> Decoder (a -> b) -> Decoder b
required key (Decoder fdecoder) = Decoder g
  where
  g :: Json -> Either String b
  g value = (fdecoder value) <*> field
    where
    field :: Either String a
    field = (decodeJson value) >>= (\obj -> (obj .: key)) >>= decodeJson

decode :: forall a. Decoder a -> Json -> Either String a
decode (Decoder f) value = f value

applyFunc :: forall a b. (a -> b) -> a -> b
applyFunc f a = f a

pipe :: forall a b. a -> (a -> b) -> b
pipe a f = f a

failed :: forall a. String -> Decoder a
failed message = Decoder (\_ -> Left message)

string :: Decoder String
string = Decoder decodeJson

bool :: Decoder Boolean
bool = Decoder decodeJson

decoder :: forall a. DecodeJson a => Decoder a
decoder = Decoder (\json -> decodeJson json)

contains :: forall a. Eq a => a -> List a -> Boolean
contains a list = findIndex (eq a) list |> Maybe.isJust

leftpad :: Int -> String -> String -> String
leftpad n pad str
  | length str >= n = str
  | otherwise = leftpad n pad (pad <> str)

filterMap :: forall a b. (a -> Maybe b) -> List a -> List b
filterMap f list = reverse res
  where
  res =
    foldl
      ( \bs a -> case f a of
          Nothing -> bs
          Just b -> b : bs
      )
      Nil
      list

joinPromises :: forall t. Deferred => Traversable t => t (Promise Unit) -> Promise Unit
joinPromises t = sequence t |> map (\_ -> unit)

infixl 7 applyFunc as <|

infixl 4 pipe as |>
