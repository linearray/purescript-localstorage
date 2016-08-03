module Test.Browser.LocalStorage (testLocalStorage) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF)
import Data.Argonaut.Core (foldJsonBoolean)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Generic (class Generic)
import Data.Maybe (Maybe (..), isJust)
import Test.Assert (ASSERT, assert)
import Unsafe.Coerce (unsafeCoerce)

import Browser.LocalStorage (GTranscode(..), STORAGE, Storage, newMockStorage)

type EffStorageRef = Eff (console :: CONSOLE, assert :: ASSERT, storage :: STORAGE, ref :: REF) Unit

newtype GItem = GItem Boolean
derive instance genericGItem :: Generic (GItem)

newtype NonGItem = NonGItem Boolean

instance eqNonGItem :: Eq NonGItem where
  eq (NonGItem item1) (NonGItem item2) = eq item1 item2

instance encodeNonGItem :: EncodeJson NonGItem where
  encodeJson (NonGItem item) = encodeJson item

instance decodeNonGItem :: DecodeJson NonGItem where
  decodeJson = map NonGItem <<< foldJsonBoolean (Left "Value is not a NonGItem") Right

data ItemKey a = GItemKey | NonGItemKey
derive instance genericItemKey :: Generic (ItemKey a)

gItemKey :: ItemKey (GTranscode GItem)
gItemKey = GItemKey

nonGItemKey :: ItemKey NonGItem
nonGItemKey = NonGItemKey

testLocalStorage :: EffStorageRef
testLocalStorage = do
  storage <- newMockStorage
  testLength (unsafeCoerce storage) 0
  testClear (unsafeCoerce storage)
  testSetItem (unsafeCoerce storage) nonGItemKey (NonGItem true)
  testSetItem (unsafeCoerce storage) gItemKey (GTranscode (GItem false))
  testRemoveItem (unsafeCoerce storage) gItemKey
  testRemoveItem (unsafeCoerce storage) gItemKey
  testGetItem (unsafeCoerce storage) nonGItemKey (Just (NonGItem true))
  testClear (unsafeCoerce storage)
  testGetItem (unsafeCoerce storage) nonGItemKey Nothing

testLength :: Storage -> Int -> EffStorageRef
testLength storage expectedLength = do
  log "Storage.length"
  length <- storage.length
  assert $ length == expectedLength

testGetItem :: forall a. (Eq a, DecodeJson a) => Storage -> ItemKey a -> Maybe a -> EffStorageRef
testGetItem storage key expectedResult = do
  log "Storage.getItem"
  result <- storage.getItem key
  assert $ result == expectedResult

testSetItem :: forall a.  (Eq a, DecodeJson a, EncodeJson a) => Storage -> ItemKey a -> a -> EffStorageRef
testSetItem storage key item = do
  log "Storage.setItem"
  length <- storage.length
  storage.setItem key item
  testGetItem (unsafeCoerce storage) key (Just item)
  testLength (unsafeCoerce storage) (length + 1)

testRemoveItem :: forall a. (Eq a, DecodeJson a) => Storage -> ItemKey a -> EffStorageRef
testRemoveItem storage key = do
  log "Storage.removeItem"
  length <- storage.length
  result <- storage.getItem key
  storage.removeItem key
  testGetItem (unsafeCoerce storage) key Nothing
  testLength (unsafeCoerce storage) (if isJust result then length - 1 else length)

testClear :: Storage -> EffStorageRef
testClear storage = do
  log "Storage.clear"
  storage.clear
  testLength (unsafeCoerce storage) 0
