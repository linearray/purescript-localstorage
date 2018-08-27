module DOM.WebStorage.Generic
( getItem
, setItem
, removeItem
--, getItemVar
) where

import Prelude

import Effect
import Data.Argonaut.Core               (stringify)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson, class DecodeRep)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson, class EncodeRep)
import Data.Argonaut.Parser             (jsonParser)
import Data.Either                      (either)
import Data.Generic.Rep                 (class Generic)
import Data.Generic.Rep.Show            (class GenericShow, genericShow)
import Data.Maybe                       (Maybe(..))

import DOM.WebStorage.ForeignStorage    (ForeignStorage)
import DOM.WebStorage.String         as String

getItem :: forall key a rep1 rep2
         . Generic (key a) rep1
        => GenericShow rep1
        => Generic a rep2
        => DecodeRep rep2
        => ForeignStorage
        -> key a
        -> Effect (Maybe a)
getItem storage key = (parse Nothing Just =<< _) <$> getItem' key
  where
    getItem' = String.getItem storage <<< genericShow

setItem :: forall key a rep1 rep2
         . Generic (key a) rep1
        => Generic a rep2
        => GenericShow rep1
        => EncodeRep rep2
        => ForeignStorage
        -> key a
        -> a
        -> Effect Unit
setItem storage key = setItem' key <<< stringify <<< genericEncodeJson
  where
    setItem' = String.setItem storage <<< genericShow

removeItem  :: forall key a rep
             . Generic (key a) rep
            => GenericShow rep
            => ForeignStorage
            -> key a
            -> Effect Unit
removeItem storage = String.removeItem storage <<< genericShow

-- getItemVar :: forall e key a. Generic (key a) => Generic a
--   => ForeignStorage -> key a -> a -> Var a
-- getItemVar storage key defaultItem = getItemVar' key stringify (parse defaultItem id) defaultItem
--   where
--     getItemVar' = String.getItemVar' storage <<< gShow

parse :: forall a b rep
       . Generic a rep
      => DecodeRep rep
      => b
      -> (a -> b)
      -> String
      -> b
parse nothing just = either (const nothing) just <<< (genericDecodeJson <=< jsonParser)
