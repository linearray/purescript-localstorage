module DOM.WebStorage.Session (getSessionStorage) where

import Control.Monad.Eff (Eff)
import DOM (DOM)

import DOM.WebStorage.Internal.Foreign (ForeignStorage)

foreign import getSessionStorage :: forall e. Eff (dom :: DOM | e) ForeignStorage
