module Spec.DOM.WebStorage.Util where

import Prelude

import Effect.Aff               (Aff)
import Effect.Class             (liftEffect)
import Data.Argonaut.Core       (caseJsonBoolean)
import Data.Argonaut.Decode     (class DecodeJson)
import Data.Argonaut.Encode     (class EncodeJson, encodeJson)
import Data.Either              (Either(..))
import Data.Generic.Rep         (class Generic)
import Data.Generic.Rep.Eq      (genericEq)
import Data.Generic.Rep.Show    (genericShow)
import Test.Spec                (Spec, it)
import Test.Spec.Assertions     (shouldEqual)

import DOM.WebStorage           (ForeignStorage, clear, length)

newtype ItemG = ItemG Boolean
derive instance genericItemG :: Generic ItemG _

instance showItemG :: Show ItemG where
  show = genericShow

instance eqItemG :: Eq ItemG where
  eq = genericEq

newtype ItemJ = ItemJ Boolean

instance showItemJ :: Show ItemJ where
  show (ItemJ item) = show item

instance eqItemJ :: Eq ItemJ where
  eq (ItemJ item1) (ItemJ item2) = item1 `eq` item2

instance encodeItemJ :: EncodeJson ItemJ where
  encodeJson (ItemJ item) = encodeJson item

instance decodeItemJ :: DecodeJson ItemJ where
  decodeJson = map ItemJ <<< caseJsonBoolean (Left "Value is not a ItemJ") Right

data ItemKey a = ItemGKey | ItemJKey
derive instance genericItemKey :: Generic (ItemKey a) _

itemGKey :: ItemKey ItemG
itemGKey = ItemGKey

itemJKey :: ItemKey ItemJ
itemJKey = ItemJKey

testLength :: forall e. ForeignStorage -> Ordering -> Int -> Aff Unit
testLength storage ordering expectedLength = do
  length <- liftEffect $ length storage
  (length `compare` expectedLength) `shouldEqual` ordering

clearSpec :: forall e. ForeignStorage -> String -> Spec Unit
clearSpec storage desc = it ("clear " <> desc) do
  liftEffect $ clear storage
  testLength storage EQ 0
