module DOM.WebStorage.String where

import Data.Function.Eff

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Prelude (Unit, pure, bind, (<$>), (<<<))

import DOM.WebStorage.Storage (ForeignStorage, STORAGE)

type Updated s b = { newValue :: s, returnValue :: b }

length :: forall e. ForeignStorage -> Eff (storage :: STORAGE | e) Int
length = runEffFn1 lengthImpl

key :: forall e. ForeignStorage -> Int -> Eff (storage :: STORAGE | e) (Maybe String)
key = runEffFn4 keyImpl Nothing Just

getItem :: forall e. ForeignStorage -> String -> Eff (storage :: STORAGE | e) (Maybe String)
getItem = runEffFn4 getItemImpl Nothing Just

setItem :: forall e. ForeignStorage -> String -> String -> Eff (storage :: STORAGE | e) Unit
setItem = runEffFn3 setItemImpl

removeItem :: forall e. ForeignStorage -> String -> Eff (storage :: STORAGE | e) Unit
removeItem = runEffFn2 removeItemImpl

clear :: forall e. ForeignStorage -> Eff (storage :: STORAGE | e) Unit
clear = runEffFn1 clearImpl

updateItem :: forall e. ForeignStorage -> String -> (Maybe String -> String) -> Eff (storage :: STORAGE | e) String
updateItem storage key' update = updateItem' storage key' update'
  where
    update' = (\newValue -> { newValue, returnValue: newValue }) <<< update

updateItem' :: forall e b. ForeignStorage -> String -> (Maybe String -> Updated String b) -> Eff (storage :: STORAGE | e) b
updateItem' storage key' update = do
  updated <- update <$> getItem storage key'
  setItem storage key' updated.newValue
  pure updated.returnValue

foreign import lengthImpl :: forall e. EffFn1 (storage :: STORAGE | e)
  ForeignStorage Int
foreign import keyImpl :: forall e. EffFn4 (storage :: STORAGE | e)
  (Maybe String) (String -> Maybe String) ForeignStorage Int (Maybe String)
foreign import getItemImpl :: forall e. EffFn4 (storage :: STORAGE | e)
  (Maybe String) (String -> Maybe String) ForeignStorage String (Maybe String)
foreign import setItemImpl :: forall e. EffFn3 (storage :: STORAGE | e)
  ForeignStorage String String Unit
foreign import removeItemImpl :: forall e. EffFn2 (storage :: STORAGE | e)
  ForeignStorage String Unit
foreign import clearImpl :: forall e. EffFn1 (storage :: STORAGE | e)
  ForeignStorage Unit
