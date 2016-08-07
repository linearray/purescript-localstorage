module Spec.DOM.WebStorage.Ref where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, writeRef, readRef)
import Data.Maybe (maybe)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import DOM.WebStorage (ForeignStorage, STORAGE, getItem, getItemRef, length, removeItem)
import Spec.DOM.WebStorage.Util (ItemG(..), clearSpec, itemGKey, testLength)

spec :: forall e. ForeignStorage -> Spec (storage :: STORAGE, ref :: REF | e) Unit
spec storage = describe "Ref" do
  clearSpec' "starts empty"
  clearSpec' "remains empty"
  setItemSpec itemGKey (ItemG false) "creates item"
  setItemSpec itemGKey (ItemG true) "updates item"
  removeItemSpec itemGKey "deletes item"
  removeItemSpec itemGKey "ignores missing item"
  setItemSpec itemGKey (ItemG true) "creates item to clear"
  clearSpec' "deletes item"
  where
    testLength' = testLength storage

    testGetItem key expectedItem = do
      itemRef <- liftEff $ getItemRef' key
      item <- liftEff $ readRef itemRef
      item `shouldEqual` expectedItem

    setItemSpec key item desc = it ("setItem " <> desc) do
      length <- liftEff $ length storage
      result <- liftEff $ getItem storage key
      itemRef <- liftEff $ getItemRef' key
      liftEff $ writeRef itemRef item
      testGetItem key item
      testLength' (maybe GT (const EQ) result) length -- increments

    removeItemSpec key desc = it ("removeItem " <> desc) do
      length <- liftEff $ length storage
      result <- liftEff $ getItem storage key
      liftEff $ removeItem storage key
      testGetItem key (ItemG false)
      testLength' (maybe EQ (const LT) result) length -- decrements

    clearSpec' = clearSpec storage

    getItemRef' key = getItemRef storage key (ItemG false)
