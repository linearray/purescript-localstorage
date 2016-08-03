module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Test.Assert (ASSERT)

import Browser.LocalStorage (STORAGE)
import Test.Browser.LocalStorage (testLocalStorage)

main :: Eff (console :: CONSOLE, assert :: ASSERT, storage :: STORAGE, ref :: REF) Unit
main = testLocalStorage