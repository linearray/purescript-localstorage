module Spec.DOM.WebStorage.Util where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut.Core (foldJsonBoolean)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Generic (class Generic, gEq, gShow)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

import DOM.WebStorage (STORAGE, ForeignStorage, clear, length)

newtype ItemG = ItemG Boolean
derive instance genericItemG :: Generic ItemG

instance showItemG :: Show ItemG where
  show = gShow

instance eqItemG :: Eq ItemG where
  eq = gEq

newtype ItemJ = ItemJ Boolean

instance showItemJ :: Show ItemJ where
  show (ItemJ item) = show item

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

testLength :: forall e. ForeignStorage -> Ordering -> Int -> Aff (storage :: STORAGE | e) Unit
testLength storage ordering expectedLength = do
  length <- liftEff $ length storage
  (length `compare` expectedLength) `shouldEqual` ordering

clearSpec :: forall e. ForeignStorage -> String -> Spec (storage :: STORAGE | e) Unit
clearSpec storage desc = it ("clear " <> desc) do
  liftEff $ clear storage
  testLength storage EQ 0
