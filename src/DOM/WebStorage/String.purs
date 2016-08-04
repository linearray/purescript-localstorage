module DOM.WebStorage.String where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe)

import DOM.WebStorage.Internal.Foreign (ForeignStorage, STORAGE, Updated)
import DOM.WebStorage.Internal.Foreign as Foreign

class StrStorage s where
  length :: forall e. s -> Eff (storage :: STORAGE | e) Int
  key :: forall e. s -> Int -> Eff (storage :: STORAGE | e) (Maybe String)
  getItem :: forall e. s -> String -> Eff (storage :: STORAGE | e) (Maybe String)
  setItem :: forall e. s -> String -> String -> Eff (storage :: STORAGE | e) Unit
  removeItem :: forall e. s -> String -> Eff (storage :: STORAGE | e) Unit
  clear :: forall e. s -> Eff (storage :: STORAGE | e) Unit

instance strStorageForeignStorage :: StrStorage ForeignStorage where
  length = Foreign.length
  key = Foreign.key
  getItem = Foreign.getItem
  setItem = Foreign.setItem
  removeItem = Foreign.removeItem
  clear = Foreign.clear

updateItem :: forall e s. StrStorage s
  => s -> String -> (Maybe String -> String) -> Eff (storage :: STORAGE | e) String
updateItem storage key update = updateItem' storage key update'
  where
    update' = (\newValue -> { newValue, returnValue: newValue }) <<< update

updateItem' :: forall e s b. StrStorage s
  => s -> String -> (Maybe String -> Updated String b) -> Eff (storage :: STORAGE | e) b
updateItem' storage key update = do
  updated <- update <$> getItem storage key
  setItem storage key updated.newValue
  pure updated.returnValue
