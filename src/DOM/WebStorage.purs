module DOM.WebStorage (module Exports) where

--import DOM (DOM) as Exportsw

import DOM.WebStorage.Generic
  ( getItem
--  , getItemVar
  , removeItem
  , setItem
  ) as Exports
import DOM.WebStorage.ForeignStorage
  ( ForeignStorage
  , getLocalStorage
  , getSessionStorage
  ) as Exports
import DOM.WebStorage.String
  ( clear
  , length
  ) as Exports
