module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Node.Process (PROCESS)
import Test.Spec (describe)
import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)

import DOM.WebStorage (STORAGE, newMockStorage)
import Spec.DOM.WebStorage.Generic as Generic
import Spec.DOM.WebStorage.JSON as JSON
import Spec.DOM.WebStorage.Ref as Ref
import Spec.DOM.WebStorage.String as String

main :: Eff (console :: CONSOLE, process :: PROCESS, storage :: STORAGE, ref :: REF) Unit
main = do
  storage <- newMockStorage
  run [ consoleReporter ] $ describe "DOM.WebStorage" do
    Generic.spec storage
    JSON.spec storage
    Ref.spec storage
    String.spec storage
