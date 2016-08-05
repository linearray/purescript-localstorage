module DOM.WebStorage.Storage where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)

foreign import data STORAGE :: !
foreign import data ForeignStorage :: *

foreign import getLocalStorage :: forall e. Eff (dom :: DOM | e) ForeignStorage
foreign import getSessionStorage :: forall e. Eff (dom :: DOM | e) ForeignStorage
foreign import newMockStorage :: forall e. Eff (ref :: REF | e) ForeignStorage
