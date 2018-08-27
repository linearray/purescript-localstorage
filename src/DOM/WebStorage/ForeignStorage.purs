module DOM.WebStorage.ForeignStorage where

import Effect

foreign import data ForeignStorage :: Type

foreign import getLocalStorage   :: Effect ForeignStorage
foreign import getSessionStorage :: Effect ForeignStorage
foreign import newMockStorage    :: Effect ForeignStorage
