module DOM.WebStorage (module Exports) where

import DOM.WebStorage.Generic
  ( getItem
  , getItemRef
  , removeItem
  , setItem
  ) as Exports
import DOM.WebStorage.Storage
  ( ForeignStorage
  , STORAGE
  , getLocalStorage
  , getSessionStorage
  , newMockStorage
  ) as Exports
import DOM.WebStorage.String
  ( clear
  , length
  ) as Exports
