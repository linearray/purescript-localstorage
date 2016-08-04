module DOM.WebStorage.Internal.Generic where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, gDecodeJson)
import Data.Argonaut.Encode (class EncodeJson, gEncodeJson)
import Data.Generic (class Generic, gCompare, gEq, gShow)

newtype TranscodeG a = TranscodeG a
derive instance genericGTransocde :: Generic a => Generic (TranscodeG a)

instance showTranscodeG :: Generic a => Show (TranscodeG a) where
  show = gShow

instance eqTranscodeG ::Generic a => Eq (TranscodeG a) where
  eq = gEq

instance ordTranscodeG :: Generic a => Ord (TranscodeG a) where
  compare = gCompare

instance encodeTranscodeG :: Generic a => EncodeJson (TranscodeG a) where
  encodeJson = gEncodeJson

instance decodeTranscodeG :: Generic a => DecodeJson (TranscodeG a) where
  decodeJson = gDecodeJson

unTranscodeG :: forall a. TranscodeG a -> a
unTranscodeG (TranscodeG item) = item
