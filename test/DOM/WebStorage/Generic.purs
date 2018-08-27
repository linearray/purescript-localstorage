module Spec.DOM.WebStorage.Generic where

import Prelude

import Effect.Class         (liftEffect)
import Data.Maybe           (Maybe(..), maybe)
import Test.Spec            (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import DOM.WebStorage (ForeignStorage, getItem, length, removeItem, setItem)
import Spec.DOM.WebStorage.Util (ItemG(..), clearSpec, itemGKey, testLength)

spec :: ForeignStorage -> Spec Unit
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
      result <- liftEffect $ getItem storage key
      result `shouldEqual` expectedResult

    setItemSpec key item desc = it ("setItem " <> desc) do
      length <- liftEffect $ length storage
      result <- liftEffect $ getItem storage key
      liftEffect $ setItem storage key item
      testGetItem key (Just item)
      testLength' (maybe GT (const EQ) result) length -- increments

    removeItemSpec key desc = it ("removeItem " <> desc) do
      length <- liftEffect $ length storage
      result <- liftEffect $ getItem storage key
      liftEffect $ removeItem storage key
      testGetItem key Nothing
      testLength' (maybe EQ (const LT) result) length -- decrements

    clearSpec' = clearSpec storage
