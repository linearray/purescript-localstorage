module DOM.WebStorage.Generic where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Argonaut.Decode (gDecodeJson)
import Data.Argonaut.Encode (gEncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (either)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))

import DOM.WebStorage.Internal (ForeignStorage, STORAGE, Updated)
import DOM.WebStorage.Internal as Internal

getItem :: forall e key a. (Generic (key a), Generic a)
  => ForeignStorage -> key a -> Eff (storage :: STORAGE | e) (Maybe a)
getItem storage key = (parse =<< _) <$> getItem' key
  where
    getItem' = Internal.getItem storage <<< gShow
    parse = either (const Nothing) Just <<< (gDecodeJson <=< jsonParser)

setItem :: forall e key a. (Generic (key a), Generic a)
  => ForeignStorage -> key a -> a -> Eff (storage :: STORAGE | e) Unit
setItem storage key item = setItem' key (stringify item)
  where
    setItem' = Internal.setItem storage <<< gShow
    stringify = show <<< gEncodeJson

removeItem :: forall e key a. Generic (key a)
  => ForeignStorage -> key a -> Eff (storage :: STORAGE | e) Unit
removeItem storage = Internal.removeItem storage <<< gShow

updateItem :: forall e key a. (Generic (key a), Generic a)
  => ForeignStorage -> key a -> (Maybe a -> a) -> Eff (storage :: STORAGE | e) a
updateItem storage key update = updateItem' storage key update'
  where
    update' = (\newValue -> { newValue, returnValue: newValue }) <<< update

updateItem' :: forall e key a b. (Generic (key a), Generic a)
  => ForeignStorage -> key a -> (Maybe a -> Updated a b) -> Eff (storage :: STORAGE | e) b
updateItem' storage key update = do
  updated <- update <$> getItem storage key
  setItem storage key updated.newValue
  pure updated.returnValue
