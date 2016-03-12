module Browser.LocalStorage.Raw where

import Prelude
import Control.Monad.Eff
import Data.Argonaut.Core as A
import Data.Argonaut.Core (JNumber, JString, Json)
import Data.Int as I
import Data.Maybe
import Data.Function
import Partial.Unsafe (unsafeCrashWith)


foreign import data STORAGE :: !
type EffLocalStorage eff = Eff (storage :: STORAGE | eff)

foreign import data LocalStorage :: *
foreign import data SessionStorage :: *

foreign import jsLocalStorage :: LocalStorage
foreign import jsSessionStorage :: SessionStorage

type Storage = {
  length :: forall eff. EffLocalStorage eff Int
, key :: forall eff. Int -> EffLocalStorage eff (Maybe String)
, getItem :: forall eff. String -> EffLocalStorage eff (Maybe String)
, setItem :: forall eff. String -> String -> EffLocalStorage eff Unit
, removeItem :: forall eff. String -> EffLocalStorage eff Unit
, clear :: forall eff. EffLocalStorage eff Unit
}

localStorage :: Storage
localStorage = {
  length : jsLength jsLocalStorage
, key : jsKey jsLocalStorage
, getItem : jsGetItem jsLocalStorage
, setItem : jsSetItem jsLocalStorage
, removeItem : jsRemoveItem jsLocalStorage
, clear : jsClear jsLocalStorage
}

sessionStorage :: Storage
sessionStorage = {
  length : jsLength jsSessionStorage
, key : jsKey jsSessionStorage
, getItem : jsGetItem jsSessionStorage
, setItem : jsSetItem jsSessionStorage
, removeItem : jsRemoveItem jsSessionStorage
, clear : jsClear jsSessionStorage
}

jsLength :: forall eff storage. storage -> EffLocalStorage eff Int
jsLength = map (fromMaybe (unsafeCrashWith "The browser should return a valid length for storage") <<< I.fromNumber)
       <<< jsUnsafeLength

jsKey :: forall eff storage. storage -> Int -> EffLocalStorage eff (Maybe String)
jsKey storage index = A.toString <$> runFn2 jsUnsafeKey storage (I.toNumber index)

jsGetItem :: forall eff storage. storage -> String -> EffLocalStorage eff (Maybe String)
jsGetItem storage key = A.toString <$> runFn2 jsUnsafeGetItem storage key

jsSetItem :: forall eff storage. storage -> String -> String -> EffLocalStorage eff Unit
jsSetItem storage key val = runFn3 jsUnsafeSetItem storage key val

jsRemoveItem :: forall eff storage. storage -> String -> EffLocalStorage eff Unit
jsRemoveItem storage key = runFn2 jsUnsafeRemoveItem storage key

jsClear :: forall eff storage. storage -> EffLocalStorage eff Unit
jsClear = jsUnsafeClear

foreign import jsUnsafeLength
  :: forall eff storage. storage -> EffLocalStorage eff JNumber

foreign import jsUnsafeKey
  :: forall eff storage. Fn2 storage JNumber (EffLocalStorage eff Json)

foreign import jsUnsafeGetItem
  :: forall eff storage. Fn2 storage JString (EffLocalStorage eff Json)

foreign import jsUnsafeSetItem
  :: forall eff storage. Fn3 storage JString JString (EffLocalStorage eff Unit)

foreign import jsUnsafeRemoveItem
  :: forall eff storage. storage JString (EffLocalStorage eff Unit)

foreign import jsUnsafeClear
  :: forall eff storage. storage -> EffLocalStorage eff Unit
