module DOM.WebStorage.JSON
( getItem
, setItem
--, getItemVar
) where

import Prelude

import Effect
import Data.Argonaut.Core       (stringify)
import Data.Argonaut.Decode     (class DecodeJson, decodeJson)
import Data.Argonaut.Encode     (class EncodeJson, encodeJson)
import Data.Argonaut.Parser     (jsonParser)
import Data.Either              (either)
import Data.Generic.Rep         (class Generic)
import Data.Generic.Rep.Show    (class GenericShow, genericShow)
import Data.Maybe               (Maybe(..))

import DOM.WebStorage.ForeignStorage (ForeignStorage)
import DOM.WebStorage.String as String

getItem :: forall key a rep
         . Generic (key a) rep
        => GenericShow rep
        => DecodeJson a
        => ForeignStorage
        -> key a
        -> Effect (Maybe a)
getItem storage key = (parse Nothing Just =<< _) <$> getItem' key
  where
    getItem' = String.getItem storage <<< genericShow

setItem :: forall key a rep
         . Generic (key a) rep
        => GenericShow rep
        => EncodeJson a
        => ForeignStorage
        -> key a
        -> a
        -> Effect Unit
setItem storage key = setItem' key <<< stringify <<< encodeJson
  where
    setItem' = String.setItem storage <<< genericShow

-- getItemVar :: forall e key a. Generic (key a) => EncodeJson a => DecodeJson a
--   => ForeignStorage -> key a -> a -> Var (storage :: STORAGE | e) a
-- getItemVar storage key defaultItem = getItemVar' key stringify (parse defaultItem id) defaultItem
--   where
--     getItemVar' = String.getItemVar' storage <<< gShow

parse :: forall a b. DecodeJson a => b -> (a -> b) -> String -> b
parse nothing just = either (const nothing) just <<< (decodeJson <=< jsonParser)
