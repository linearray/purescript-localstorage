module Spec.DOM.WebStorage.Var where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff) as Eff
import Control.Monad.Eff.Var (get, ($=))
import Data.Maybe (maybe)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import DOM.WebStorage (ForeignStorage, STORAGE, getItem, getItemVar, length, removeItem)
import Spec.DOM.WebStorage.Util (ItemG(..), clearSpec, itemGKey, testLength)

spec :: forall e. ForeignStorage -> Spec (storage :: STORAGE | e) Unit
spec storage = describe "Var" do
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
      item <- liftEff $ get (getItemVar' key)
      item `shouldEqual` expectedItem

    setItemSpec key item desc = it ("setItem " <> desc) do
      length <- liftEff $ length storage
      result <- liftEff $ getItem storage key
      liftEff $ getItemVar' key $= item
      testGetItem key item
      testLength' (maybe GT (const EQ) result) length -- increments

    removeItemSpec key desc = it ("removeItem " <> desc) do
      length <- liftEff $ length storage
      result <- liftEff $ getItem storage key
      liftEff $ removeItem storage key
      testGetItem key (ItemG false)
      testLength' (maybe EQ (const LT) result) length -- decrements

    clearSpec' = clearSpec storage

    getItemVar' key = getItemVar storage key (ItemG false)

    -- purescript/purescript#2061
    liftEff = Eff.liftEff :: forall eff a. Eff eff a -> Aff eff a
