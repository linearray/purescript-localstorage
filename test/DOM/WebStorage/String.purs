module Spec.DOM.WebStorage.String where

import Prelude

import Effect.Class         (liftEffect)
import Data.Maybe           (Maybe(..), maybe)
import Test.Spec            (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import DOM.WebStorage           (ForeignStorage, length)
import DOM.WebStorage.String    (getItem, removeItem, setItem)
import Spec.DOM.WebStorage.Util (clearSpec, testLength)

spec :: forall e. ForeignStorage -> Spec Unit
spec storage = describe "String" do
  clearSpec' "starts empty"
  clearSpec' "remains empty"
  setItemSpec "itemKey" "false" "creates item"
  setItemSpec "itemKey" "true" "updates item"
  removeItemSpec "itemKey" "deletes item"
  removeItemSpec "itemKey" "ignores missing item"
  setItemSpec "itemKey" "true" "creates item to clear"
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
