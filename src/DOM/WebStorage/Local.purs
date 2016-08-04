module DOM.WebStorage.Local
( LocalStorage
, getLocalStorage
) where

import DOM.WebStorage.Internal.Foreign

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Prelude ((<$>), (<<<))

import DOM.WebStorage.String (class StrStorage)

newtype LocalStorage = LocalStorage ForeignStorage

instance strStorageLocalStorage :: StrStorage LocalStorage where
  length = length <<< unLocalStorage
  key = key <<< unLocalStorage
  getItem = getItem <<< unLocalStorage
  setItem = setItem <<< unLocalStorage
  removeItem = removeItem <<< unLocalStorage
  clear = clear <<< unLocalStorage

unLocalStorage :: LocalStorage -> ForeignStorage
unLocalStorage (LocalStorage storage) = storage

getLocalStorage :: forall e. Eff (dom :: DOM | e) LocalStorage
getLocalStorage = LocalStorage <$> localStorageImpl

foreign import localStorageImpl :: forall e. Eff (dom :: DOM | e) ForeignStorage
