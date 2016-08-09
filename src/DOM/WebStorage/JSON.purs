module DOM.WebStorage.JSON
( getItem
, setItem
, getItemVar
) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Var (Var)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (either)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))

import DOM.WebStorage.Storage (ForeignStorage, STORAGE)
import DOM.WebStorage.String as String

getItem :: forall e key a. (Generic (key a), DecodeJson a)
  => ForeignStorage -> key a -> Eff (storage :: STORAGE | e) (Maybe a)
getItem storage key = (parse Nothing Just =<< _) <$> getItem' key
  where
    getItem' = String.getItem storage <<< gShow

setItem :: forall e key a. (Generic (key a), EncodeJson a)
  => ForeignStorage -> key a -> a -> Eff (storage :: STORAGE | e) Unit
setItem storage key = setItem' key <<< stringify
  where
    setItem' = String.setItem storage <<< gShow

getItemVar :: forall e key a. (Generic (key a), EncodeJson a, DecodeJson a)
  => ForeignStorage -> key a -> a -> Var (storage :: STORAGE | e) a
getItemVar storage key defaultItem = getItemVar' key stringify (parse defaultItem id) defaultItem
  where
    getItemVar' = String.getItemVar' storage <<< gShow

stringify :: forall a. EncodeJson a => a -> String
stringify = show <<< encodeJson

parse :: forall a b. DecodeJson a => b -> (a -> b) -> String -> b
parse nothing just = either (const nothing) just <<< (decodeJson <=< jsonParser)
