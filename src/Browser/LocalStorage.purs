module Browser.LocalStorage (
  Storage
, localStorage
, sessionStorage
, STORAGE
) where

import Prelude
import Browser.LocalStorage.Raw as Raw
import Browser.LocalStorage.Raw (EffLocalStorage)
import Control.Bind ((<=<))
import Control.Monad.Eff
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Maybe
import Data.Either
import Data.Generic

type STORAGE = Raw.STORAGE

type Storage = {
  length :: forall eff. EffLocalStorage eff Int
-- , key :: forall eff key a. Generic (key a) => Int -> EffLocalStorage eff (Maybe (key a))
, getItem :: forall eff key a. (Generic (key a), Generic a) =>  key a -> EffLocalStorage eff (Maybe a)
, setItem :: forall eff key a. (Generic (key a), Generic a) => key a -> a -> EffLocalStorage eff Unit
, removeItem :: forall eff key a. Generic (key a) => key a -> EffLocalStorage eff Unit
, clear :: forall eff. EffLocalStorage eff Unit
}

mkStorage :: Raw.Storage -> Storage
mkStorage raw = {
  length : raw.length
-- , key = raw.key -- I don't think we need that and we would need a gRead to support it.
, getItem : map (>>= hushGenericDecode) <<< raw.getItem <<< gShow
, setItem : \key val -> raw.setItem (gShow key) (show $ gEncodeJson val)
, removeItem : raw.removeItem <<< gShow
, clear : raw.clear
}

-- With type annotation this module does not compile! Compiler bug?
-- localStorage :: Storage
localStorage = mkStorage Raw.localStorage

-- With type annotation this module does not compile! Compiler bug?
-- sessionStorage :: Storage
sessionStorage = mkStorage Raw.sessionStorage

hushGenericDecode :: forall a. Generic a => String -> Maybe a
hushGenericDecode = hush <<< (gDecodeJson <=< jsonParser)

hush :: forall e a. Either e a -> Maybe a
hush (Left _) = Nothing
hush (Right a) = Just a
