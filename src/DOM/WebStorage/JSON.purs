module DOM.WebStorage.JSON where

import Prelude

import Control.Monad.Eff (Eff)
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
getItem storage key = (parse =<< _) <$> getItem' key
  where
    getItem' = String.getItem storage <<< gShow
    parse = either (const Nothing) Just <<< (decodeJson <=< jsonParser)

setItem :: forall e key a. (Generic (key a), EncodeJson a)
  => ForeignStorage -> key a -> a -> Eff (storage :: STORAGE | e) Unit
setItem storage key = setItem' key <<< stringify
  where
    setItem' = String.setItem storage <<< gShow
    stringify = show <<< encodeJson

updateItem :: forall e key a. (Generic (key a), EncodeJson a, DecodeJson a)
  => ForeignStorage -> key a -> (Maybe a -> a) -> Eff (storage :: STORAGE | e) a
updateItem storage key update = updateItem' storage key update'
  where
    update' = (\newValue -> { newValue, returnValue: newValue }) <<< update

updateItem' :: forall e key a b. (Generic (key a), EncodeJson a, DecodeJson a)
  => ForeignStorage -> key a -> (Maybe a -> String.Updated a b)
  -> Eff (storage :: STORAGE | e) b
updateItem' storage key update = do
  updated <- update <$> getItem storage key
  setItem storage key updated.newValue
  pure updated.returnValue
