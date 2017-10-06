module DOM.WebStorage.ForeignStorage where

import Control.Monad.Eff (Eff, kind Effect)
import DOM (DOM)

foreign import data STORAGE :: Effect
foreign import data MOCK :: Effect
foreign import data ForeignStorage :: Type

foreign import getLocalStorage :: forall e. Eff (dom :: DOM | e) ForeignStorage
foreign import getSessionStorage :: forall e. Eff (dom :: DOM | e) ForeignStorage
foreign import newMockStorage :: forall e. Eff (mock :: MOCK | e) ForeignStorage
