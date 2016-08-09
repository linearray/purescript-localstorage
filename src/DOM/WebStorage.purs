module DOM.WebStorage (module Exports) where

import DOM (DOM) as Exports
import DOM.WebStorage.Generic
  ( getItem
  , getItemVar
  , removeItem
  , setItem
  ) as Exports
import DOM.WebStorage.Storage
  ( ForeignStorage
  , STORAGE
  , getLocalStorage
  , getSessionStorage
  ) as Exports
import DOM.WebStorage.String
  ( clear
  , length
  ) as Exports
