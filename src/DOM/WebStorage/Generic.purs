module DOM.WebStorage.Generic
( getItem
, setItem
, removeItem
, getItemRef
) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref)
import Data.Argonaut.Decode (gDecodeJson)
import Data.Argonaut.Encode (gEncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (either)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))

import DOM.WebStorage.Storage (ForeignStorage, STORAGE)
import DOM.WebStorage.String as String

getItem :: forall e key a. (Generic (key a), Generic a)
  => ForeignStorage -> key a -> Eff (storage :: STORAGE | e) (Maybe a)
getItem storage key = (parse Nothing Just =<< _) <$> getItem' key
  where
    getItem' = String.getItem storage <<< gShow

setItem :: forall e key a. (Generic (key a), Generic a)
  => ForeignStorage -> key a -> a -> Eff (storage :: STORAGE | e) Unit
setItem storage key = setItem' key <<< stringify
  where
    setItem' = String.setItem storage <<< gShow

removeItem :: forall e key a. Generic (key a)
  => ForeignStorage -> key a -> Eff (storage :: STORAGE | e) Unit
removeItem storage = String.removeItem storage <<< gShow

getItemRef :: forall e key a. (Generic (key a), Generic a)
  => ForeignStorage -> key a -> a -> Eff (storage :: STORAGE | e) (Ref a)
getItemRef storage key defaultItem = getItemRef' key defaultItem
  where
    getItemRef' key = String.getItemRef' storage (gShow key) stringify (parse defaultItem id)

stringify :: forall a. Generic a => a -> String
stringify = show <<< gEncodeJson

parse :: forall a b. Generic a => b -> (a -> b) -> String -> b
parse nothing just = either (const nothing) just <<< (gDecodeJson <=< jsonParser)
