module DOM.WebStorage.Mock (newMockStorage) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)

import DOM.WebStorage.Internal.Foreign (ForeignStorage)

foreign import newMockStorage :: forall e. Eff (ref :: REF | e) ForeignStorage
