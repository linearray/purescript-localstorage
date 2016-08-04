module DOM.WebStorage.Internal.Foreign where

import Data.Function.Eff

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Prelude (Unit)

type Updated s b = { newValue :: s, returnValue :: b }

foreign import data STORAGE :: !
foreign import data ForeignStorage :: *

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
