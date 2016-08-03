module Browser.LocalStorage
( module Exports
, Storage
, GTranscode(..)
, unGTranscode
, getLocalStorage
, getSessionStorage
, newMockStorage
, mkStorage
) where

import Prelude

import Control.Bind ((<=<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, gDecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, gEncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (either)
import Data.Generic (class Generic, gCompare, gEq, gShow)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import Unsafe.Coerce (unsafeCoerce)

import Browser.LocalStorage.Raw as Raw
import Browser.LocalStorage.Raw (STORAGE) as Exports

type Storage =
  { length :: forall e. Raw.EffStorage e Int
  -- , key :: forall e key a. Generic (key a) => Int -> Raw.EffStorage e (Maybe (key a)) -- would require gRead
  , getItem :: forall e key a. (Generic (key a), DecodeJson a) => key a -> Raw.EffStorage e (Maybe a)
  , setItem :: forall e key a. (Generic (key a), EncodeJson a) => key a -> a -> Raw.EffStorage e Unit
  , removeItem :: forall e key a. Generic (key a) => key a -> Raw.EffStorage e Unit
  , clear :: forall e. Raw.EffStorage e Unit
  }

newtype GTranscode a = GTranscode a
derive instance genericGTransocde :: Generic a => Generic (GTranscode a)

instance showGTranscode :: Generic a => Show (GTranscode a) where
  show = gShow

instance eqGTranscode :: Generic a => Eq (GTranscode a) where
  eq = gEq

instance ordGTranscode :: Generic a => Ord (GTranscode a) where
  compare = gCompare

instance encodeGTranscode :: Generic a => EncodeJson (GTranscode a) where
  encodeJson = gEncodeJson

instance decodeGTranscode :: Generic a => DecodeJson (GTranscode a) where
  decodeJson = gDecodeJson

unGTranscode :: forall a. GTranscode a -> a
unGTranscode (GTranscode item) = item

-- https://github.com/purescript/purescript/issues/1957
getLocalStorage :: forall e. Eff (storage :: Raw.STORAGE, dom :: DOM | e) Storage
getLocalStorage = unsafeCoerce <<< mkStorage <$> Raw.getLocalStorage

-- also https://github.com/purescript/purescript/issues/2229
getSessionStorage :: forall e. Eff (storage :: Raw.STORAGE, dom :: DOM | e) Storage
getSessionStorage = unsafeCoerce <<< mkStorage <$> Raw.getSessionStorage

newMockStorage :: forall e. Eff (storage :: Raw.STORAGE, ref :: REF | e) Storage
newMockStorage = unsafeCoerce <<< mkStorage <$> Raw.newMockStorage

mkStorage :: Raw.Storage -> Storage
mkStorage raw =
  { length: raw.length
  , getItem:
      let parse = either (const Nothing) Just <<< (decodeJson <=< jsonParser)
      in map (parse =<< _) <<< raw.getItem <<< gShow
  , setItem: \key item ->
      let stringify = show <<< encodeJson
      in raw.setItem (gShow key) (stringify item)
  , removeItem: raw.removeItem <<< gShow
  , clear: raw.clear
  }
