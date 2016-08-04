module Test.DOM.WebStorage (testJSONStorage) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF)
import Data.Argonaut.Core (foldJsonBoolean)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Generic (class Generic, gEq)
import Data.Maybe (Maybe(..), maybe)
import Test.Assert (ASSERT, assert)

import DOM.WebStorage
  ( class JSONStorage
  , TranscodeG
  , STORAGE
  , clear
  , removeItem
  , getItem
  , length
  , gUpdateItem
  , updateItem
  , gSetItem
  , setItem
  , gGetItem
  , newMockStorage
  )

type EffStorageRef = Eff (console :: CONSOLE, assert :: ASSERT, storage :: STORAGE, ref :: REF) Unit

newtype ItemG = ItemG Boolean
derive instance genericItemG :: Generic ItemG

instance eqItemG :: Eq ItemG where
  eq = gEq

newtype ItemN = ItemN Boolean

instance eqItemN :: Eq ItemN where
  eq (ItemN item1) (ItemN item2) = item1 `eq` item2

instance encodeItemN :: EncodeJson ItemN where
  encodeJson (ItemN item) = encodeJson item

instance decodeItemN :: DecodeJson ItemN where
  decodeJson = map ItemN <<< foldJsonBoolean (Left "Value is not a ItemN") Right

data ItemKey a = ItemGKey | ItemNKey
derive instance genericItemKey :: Generic (ItemKey a)

itemGKey :: ItemKey (TranscodeG ItemG)
itemGKey = ItemGKey

itemNKey :: ItemKey ItemN
itemNKey = ItemNKey

testJSONStorage :: EffStorageRef
testJSONStorage = do
  storage <- newMockStorage
  testLength storage 0
  testClear storage
  testSetItem storage itemNKey (ItemN false)
  testUpdateItem storage itemNKey (const (ItemN (true)))
  testGSetItem storage itemGKey (ItemG true)
  testGUpdateItem storage itemGKey (const (ItemG (false)))
  testRemoveItem storage itemGKey
  testRemoveItem storage itemGKey
  testGetItem storage itemNKey (Just (ItemN true))
  testClear storage
  testGetItem storage itemNKey Nothing

testLength :: forall s. JSONStorage s => s -> Int -> EffStorageRef
testLength storage expectedLength = do
  log "length"
  length <- length storage
  assert $ length == expectedLength

testGetItem :: forall s a. (JSONStorage s, Eq a, DecodeJson a)
  => s -> ItemKey a -> Maybe a -> EffStorageRef
testGetItem storage key expectedResult = do
  log "getItem"
  result <- getItem storage key
  assert $ result == expectedResult

testGGetItem :: forall s a. (JSONStorage s, Eq a, Generic a)
  => s -> ItemKey (TranscodeG a) -> Maybe a -> EffStorageRef
testGGetItem storage key expectedResult = do
  log "gGetItem"
  result <- gGetItem storage key
  assert $ result == expectedResult

testSetItem :: forall s a.  (JSONStorage s, Eq a, DecodeJson a, EncodeJson a)
  => s -> ItemKey a -> a -> EffStorageRef
testSetItem storage key item = do
  log "setItem"
  length <- length storage
  result <- getItem storage key
  setItem storage key item
  testGetItem storage key (Just item)
  testLength storage $ maybe (length + 1) (const length) result

testGSetItem :: forall s a.  (JSONStorage s, Eq a, Generic a)
  => s -> ItemKey (TranscodeG a) -> a -> EffStorageRef
testGSetItem storage key item = do
  log "gSetItem"
  length <- length storage
  result <- getItem storage key
  gSetItem storage key item
  testGGetItem storage key (Just item)
  testLength storage $ maybe (length + 1) (const length) result

testUpdateItem :: forall s a.  (JSONStorage s, Eq a, DecodeJson a, EncodeJson a)
  => s -> ItemKey a -> (Maybe a -> a) -> EffStorageRef
testUpdateItem storage key update = do
  log "updateItem"
  length <- length storage
  result <- getItem storage key
  updateItem storage key update
  testGetItem storage key (Just (update result))
  testLength storage $ maybe (length + 1) (const length) result

testGUpdateItem :: forall s a.  (JSONStorage s, Eq a, Generic a)
  => s -> ItemKey (TranscodeG a) -> (Maybe a -> a) -> EffStorageRef
testGUpdateItem storage key update = do
  log "gUpdateItem"
  length <- length storage
  result <- gGetItem storage key
  gUpdateItem storage key update
  testGGetItem storage key (Just (update result))
  testLength storage $ maybe (length + 1) (const length) result

testRemoveItem :: forall s a. (JSONStorage s, Eq a, DecodeJson a)
  => s -> ItemKey a -> EffStorageRef
testRemoveItem storage key = do
  log "removeItem"
  length <- length storage
  result <- getItem storage key
  removeItem storage key
  testGetItem storage key Nothing
  testLength storage $ maybe length (const (length - 1)) result

testClear :: forall s. JSONStorage s => s -> EffStorageRef
testClear storage = do
  log "clear"
  clear storage
  testLength storage 0
