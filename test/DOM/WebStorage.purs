module Test.DOM.WebStorage (testWebStorage) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, modifyRef')
import Data.Argonaut.Core (foldJsonBoolean)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Generic (class Generic, gEq)
import Data.Maybe (Maybe(..), maybe)
import Test.Assert (ASSERT, assert)

import DOM.WebStorage
  ( ForeignStorage
  , STORAGE
  , clear
  , getItem
  , getItemRef
  , length
  , newMockStorage
  , removeItem
  , setItem
  , updateItem
  )
import DOM.WebStorage.JSON as JSONStorage

type EffStorageRef = Eff (console :: CONSOLE, assert :: ASSERT, storage :: STORAGE, ref :: REF) Unit

newtype ItemG = ItemG Boolean
derive instance genericItemG :: Generic ItemG

instance eqItemG :: Eq ItemG where
  eq = gEq

newtype ItemJ = ItemJ Boolean

instance eqItemJ :: Eq ItemJ where
  eq (ItemJ item1) (ItemJ item2) = item1 `eq` item2

instance encodeItemJ :: EncodeJson ItemJ where
  encodeJson (ItemJ item) = encodeJson item

instance decodeItemJ :: DecodeJson ItemJ where
  decodeJson = map ItemJ <<< foldJsonBoolean (Left "Value is not a ItemJ") Right

data ItemKey a = ItemGKey | ItemJKey
derive instance genericItemKey :: Generic (ItemKey a)

itemGKey :: ItemKey ItemG
itemGKey = ItemGKey

itemJKey :: ItemKey ItemJ
itemJKey = ItemJKey

testWebStorage :: EffStorageRef
testWebStorage = do
  storage <- newMockStorage
  testLength storage 0
  testClear storage
  testSetItemJ storage itemJKey (ItemJ false)
  testUpdateItemJ storage itemJKey (const (ItemJ (true)))
  testSetItemG storage itemGKey (ItemG true)
  do -- FIXME: break up into suites
    log "GenericStorage.getItemRef"
    atom <- getItemRef storage itemGKey (ItemG false)
    let update state@(ItemG flag) = { state, value: flag}
    wasTrue <- modifyRef' atom update
    assert wasTrue
  testUpdateItemG storage itemGKey (const (ItemG (false)))
  testRemoveItem storage itemGKey
  testRemoveItem storage itemGKey
  testGetItemJ storage itemJKey (Just (ItemJ true))
  testClear storage
  testGetItemJ storage itemJKey Nothing

testLength :: ForeignStorage -> Int -> EffStorageRef
testLength storage expectedLength = do
  log "GenericStorage.length"
  length <- length storage
  assert $ length == expectedLength

testGetItemG :: forall a. (Eq a, Generic a)
  => ForeignStorage -> ItemKey a -> Maybe a -> EffStorageRef
testGetItemG storage key expectedResult = do
  log "GenericStorage.getItem"
  result <- getItem storage key
  assert $ result == expectedResult

testGetItemJ :: forall a. (Eq a, DecodeJson a)
  => ForeignStorage -> ItemKey a -> Maybe a -> EffStorageRef
testGetItemJ storage key expectedResult = do
  log "JSONStorage.getItem"
  result <- JSONStorage.getItem storage key
  assert $ result == expectedResult

testSetItemG :: forall a. (Eq a, Generic a)
  => ForeignStorage -> ItemKey a -> a -> EffStorageRef
testSetItemG storage key item = do
  log "GenericStorage.setItem"
  length <- length storage
  result <- getItem storage key
  setItem storage key item
  testGetItemG storage key (Just item)
  testLength storage $ maybe (length + 1) (const length) result

testSetItemJ :: forall a. (Eq a, DecodeJson a, EncodeJson a)
  => ForeignStorage -> ItemKey a -> a -> EffStorageRef
testSetItemJ storage key item = do
  log "JSONStorage.setItem"
  length <- length storage
  result <- JSONStorage.getItem storage key
  JSONStorage.setItem storage key item
  testGetItemJ storage key (Just item)
  testLength storage $ maybe (length + 1) (const length) result

testUpdateItemG :: forall a. (Eq a, Generic a)
  => ForeignStorage -> ItemKey a -> (Maybe a -> a) -> EffStorageRef
testUpdateItemG storage key update = do
  log "GenericStorage.UpdateItem"
  length <- length storage
  result <- getItem storage key
  updateItem storage key update
  testGetItemG storage key (Just (update result))
  testLength storage $ maybe (length + 1) (const length) result

testUpdateItemJ :: forall a. (Eq a, DecodeJson a, EncodeJson a)
  => ForeignStorage -> ItemKey a -> (Maybe a -> a) -> EffStorageRef
testUpdateItemJ storage key update = do
  log "JSONStorage.updateItem"
  length <- length storage
  result <- JSONStorage.getItem storage key
  JSONStorage.updateItem storage key update
  testGetItemJ storage key (Just (update result))
  testLength storage $ maybe (length + 1) (const length) result

testRemoveItem :: forall a. (Eq a, Generic a)
  => ForeignStorage -> ItemKey a -> EffStorageRef
testRemoveItem storage key = do
  log "GenericStorage.removeItem"
  length <- length storage
  result <- getItem storage key
  removeItem storage key
  testGetItemG storage key Nothing
  testLength storage $ maybe length (const (length - 1)) result

testClear :: ForeignStorage -> EffStorageRef
testClear storage = do
  log "GenericStorage.clear"
  clear storage
  testLength storage 0
