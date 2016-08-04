module DOM.WebStorage (module Exports) where

import DOM.WebStorage.Internal.Foreign (STORAGE) as Exports
import DOM.WebStorage.Internal.Generic (TranscodeG) as Exports
import DOM.WebStorage.JSON
  ( class JSONStorage
  , clear
  , gGetItem
  , gSetItem
  , gUpdateItem
  , gUpdateItem'
  , getItem
  , length
  , removeItem
  , setItem
  , updateItem
  , updateItem'
  ) as Exports
import DOM.WebStorage.Local (LocalStorage, getLocalStorage) as Exports
import DOM.WebStorage.Mock (MockStorage, newMockStorage) as Exports
import DOM.WebStorage.Session (SessionStorage, getSessionStorage) as Exports
