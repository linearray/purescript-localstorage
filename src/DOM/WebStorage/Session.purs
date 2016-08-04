module DOM.WebStorage.Session
( SessionStorage
, getSessionStorage
) where

import DOM.WebStorage.Internal.Foreign

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Prelude ((<$>), (<<<))

import DOM.WebStorage.String (class StrStorage)

newtype SessionStorage = SessionStorage ForeignStorage

instance strStorageSessionStorage :: StrStorage SessionStorage where
  length = length <<< unSessionStorage
  key = key <<< unSessionStorage
  getItem = getItem <<< unSessionStorage
  setItem = setItem <<< unSessionStorage
  removeItem = removeItem <<< unSessionStorage
  clear = clear <<< unSessionStorage

unSessionStorage :: SessionStorage -> ForeignStorage
unSessionStorage (SessionStorage storage) = storage

getSessionStorage :: forall e. Eff (dom :: DOM | e) SessionStorage
getSessionStorage = SessionStorage <$> sessionStorageImpl

foreign import sessionStorageImpl :: forall e. Eff (dom :: DOM | e) ForeignStorage
