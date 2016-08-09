module Spec.DOM.WebStorage.String where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..), maybe)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import DOM.WebStorage (ForeignStorage, STORAGE, length)
import DOM.WebStorage.String (getItem, removeItem, setItem)
import Spec.DOM.WebStorage.Util (clearSpec, testLength)

spec :: forall e. ForeignStorage -> Spec (storage :: STORAGE | e) Unit
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
