module Spec.DOM.WebStorage.Generic where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..), maybe)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import DOM.WebStorage (ForeignStorage, STORAGE, getItem, length, removeItem, setItem)
import Spec.DOM.WebStorage.Util (ItemG(..), clearSpec, itemGKey, testLength)

spec :: forall e. ForeignStorage -> Spec (storage :: STORAGE | e) Unit
spec storage = describe "Generic" do
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

    testGetItem key expectedResult = do
      result <- liftEff $ getItem storage key
      result `shouldEqual` expectedResult

    setItemSpec key item desc = it ("setItem " <> desc) do
      length <- liftEff $ length storage
      result <- liftEff $ getItem storage key
      liftEff $ setItem storage key item
      testGetItem key (Just item)
      testLength' (maybe GT (const EQ) result) length -- increments

    removeItemSpec key desc = it ("removeItem " <> desc) do
      length <- liftEff $ length storage
      result <- liftEff $ getItem storage key
      liftEff $ removeItem storage key
      testGetItem key Nothing
      testLength' (maybe EQ (const LT) result) length -- decrements

    clearSpec' = clearSpec storage
