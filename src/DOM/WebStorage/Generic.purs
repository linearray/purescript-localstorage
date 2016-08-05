module DOM.WebStorage.Generic where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Argonaut.Decode (gDecodeJson)
import Data.Argonaut.Encode (gEncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (either)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))

import DOM.WebStorage.Internal (STORAGE, Updated)
import DOM.WebStorage.String (class StrStorage)
import DOM.WebStorage.String as StrStorage

class GenericStorage s where
  length :: forall e. s -> Eff (storage :: STORAGE | e) Int
  -- key would require gRead
  getItem :: forall e key a. (Generic (key a), Generic a)
    => s -> key a -> Eff (storage :: STORAGE | e) (Maybe a)
  setItem :: forall e key a. (Generic (key a), Generic a)
    => s -> key a -> a -> Eff (storage :: STORAGE | e) Unit
  removeItem :: forall e key a. Generic (key a)
    => s -> key a -> Eff (storage :: STORAGE | e) Unit
  clear :: forall e. s -> Eff (storage :: STORAGE | e) Unit

instance genericStorageStrStorage :: StrStorage s => GenericStorage s where
  length = StrStorage.length
  getItem storage key = (parse =<< _) <$> getItem' key
    where
      getItem' = StrStorage.getItem storage <<< gShow
      parse = either (const Nothing) Just <<< (gDecodeJson <=< jsonParser)
  setItem storage key item = setItem' key (stringify item)
    where
      setItem' = StrStorage.setItem storage <<< gShow
      stringify = show <<< gEncodeJson
  removeItem storage = StrStorage.removeItem storage <<< gShow
  clear = StrStorage.clear

updateItem :: forall s e key a. (StrStorage s, Generic (key a), Generic a)
  => s -> key a -> (Maybe a -> a) -> Eff (storage :: STORAGE | e) a
updateItem storage key update = updateItem' storage key update'
  where
    update' = (\newValue -> { newValue, returnValue: newValue }) <<< update

updateItem' :: forall s e key a b. (StrStorage s, Generic (key a), Generic a)
  => s -> key a -> (Maybe a -> Updated a b) -> Eff (storage :: STORAGE | e) b
updateItem' storage key update = do
  updated <- update <$> getItem storage key
  setItem storage key updated.newValue
  pure updated.returnValue
