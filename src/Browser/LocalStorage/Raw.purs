module Browser.LocalStorage.Raw
( Storage
, EffStorage
, STORAGE
, ForeignStorage
, getLocalStorage
, getSessionStorage
, mkStorage
) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import DOM (DOM)

type Storage =
  { length :: forall e. EffStorage e Int
  , key :: forall e. Int -> EffStorage e (Maybe String)
  , getItem :: forall e. String -> EffStorage e (Maybe String)
  , setItem :: forall e. String -> String -> EffStorage e Unit
  , removeItem :: forall e. String -> EffStorage e Unit
  , clear :: forall e. EffStorage e Unit
  }
type EffStorage e = Eff (storage :: STORAGE | e)
type EffDOMStorage e = Eff (storage :: STORAGE, dom :: DOM | e)

foreign import data STORAGE :: !
foreign import data ForeignStorage :: *

foreign import localStorageImpl :: forall e. EffDOMStorage e ForeignStorage
foreign import sessionStorageImpl :: forall e. EffDOMStorage e ForeignStorage
foreign import mkStorageImpl :: forall a. Maybe a -> (a -> Maybe a) -> ForeignStorage -> Storage

getLocalStorage :: forall e. EffDOMStorage e Storage
getLocalStorage = mkStorage <$> localStorageImpl

getSessionStorage :: forall e. EffDOMStorage e Storage
getSessionStorage = mkStorage <$> sessionStorageImpl

mkStorage :: ForeignStorage -> Storage
mkStorage = mkStorageImpl Nothing Just
