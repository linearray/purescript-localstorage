module DOM.WebStorage.Mock
( MockStorage
, newMockStorage
) where

import DOM.WebStorage.Internal.Foreign

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Prelude ((<$>), (<<<))

import DOM.WebStorage.String (class StrStorage)

newtype MockStorage = MockStorage ForeignStorage

instance strStorageMockStorage :: StrStorage MockStorage where
  length = length <<< unMockStorage
  key = key <<< unMockStorage
  getItem = getItem <<< unMockStorage
  setItem = setItem <<< unMockStorage
  removeItem = removeItem <<< unMockStorage
  clear = clear <<< unMockStorage

unMockStorage :: MockStorage -> ForeignStorage
unMockStorage (MockStorage storage) = storage

newMockStorage :: forall e. Eff (ref :: REF | e) MockStorage
newMockStorage = MockStorage <$> mockStorageImpl

foreign import mockStorageImpl :: forall e. Eff (ref :: REF | e) ForeignStorage
