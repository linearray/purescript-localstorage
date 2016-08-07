module Spec.DOM.WebStorage.JSON where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..), maybe)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import DOM.WebStorage (ForeignStorage, STORAGE, length, removeItem)
import DOM.WebStorage.JSON (getItem, setItem)
import Spec.DOM.WebStorage.Util (ItemJ(..), clearSpec, itemJKey, testLength)

spec :: forall e. ForeignStorage -> Spec (storage :: STORAGE | e) Unit
spec storage = describe "JSON" do
  clearSpec' "starts empty"
  clearSpec' "remains empty"
  setItemSpec itemJKey (ItemJ false) "creates item"
  setItemSpec itemJKey (ItemJ true) "updates item"
  removeItemSpec itemJKey "deletes item"
  removeItemSpec itemJKey "ignores missing item"
  setItemSpec itemJKey (ItemJ true) "creates item to clear"
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
