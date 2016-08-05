module DOM.WebStorage (module Exports) where

import DOM.WebStorage.Internal.Foreign (ForeignStorage, STORAGE) as Exports
import DOM.WebStorage.Generic
  ( class GenericStorage
  , clear
  , getItem
  , length
  , removeItem
  , setItem
  , updateItem
  , updateItem'
  ) as Exports
import DOM.WebStorage.Local (getLocalStorage) as Exports
import DOM.WebStorage.Mock (newMockStorage) as Exports
import DOM.WebStorage.Session (getSessionStorage) as Exports
