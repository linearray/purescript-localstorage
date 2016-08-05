module DOM.WebStorage (module Exports) where

import DOM.WebStorage.Generic
  ( getItem
  , removeItem
  , setItem
  , updateItem
  , updateItem'
  ) as Exports
import DOM.WebStorage.Internal (ForeignStorage, STORAGE) as Exports
import DOM.WebStorage.Local (getLocalStorage) as Exports
import DOM.WebStorage.Mock (newMockStorage) as Exports
import DOM.WebStorage.Session (getSessionStorage) as Exports
import DOM.WebStorage.String
  ( clear
  , length
  ) as Exports
