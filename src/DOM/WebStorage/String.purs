module DOM.WebStorage.String where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe)

import DOM.WebStorage.Internal (ForeignStorage, STORAGE, Updated)
import DOM.WebStorage.Internal as Internal

length :: forall e. ForeignStorage -> Eff (storage :: STORAGE | e) Int
length = Internal.length

key :: forall e. ForeignStorage -> Int -> Eff (storage :: STORAGE | e) (Maybe String)
key = Internal.key

getItem :: forall e. ForeignStorage -> String -> Eff (storage :: STORAGE | e) (Maybe String)
getItem = Internal.getItem

setItem :: forall e. ForeignStorage -> String -> String -> Eff (storage :: STORAGE | e) Unit
setItem = Internal.setItem

removeItem :: forall e. ForeignStorage -> String -> Eff (storage :: STORAGE | e) Unit
removeItem = Internal.removeItem

clear :: forall e. ForeignStorage -> Eff (storage :: STORAGE | e) Unit
clear = Internal.clear

updateItem :: forall e
   . ForeignStorage -> String -> (Maybe String -> String)
  -> Eff (storage :: STORAGE | e) String
updateItem storage key' update = updateItem' storage key' update'
  where
    update' = (\newValue -> { newValue, returnValue: newValue }) <<< update

updateItem' :: forall e b
   . ForeignStorage -> String -> (Maybe String -> Updated String b)
  -> Eff (storage :: STORAGE | e) b
updateItem' storage key' update = do
  updated <- update <$> getItem storage key'
  setItem storage key' updated.newValue
  pure updated.returnValue
