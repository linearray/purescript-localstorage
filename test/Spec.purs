module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Node.Process (PROCESS)
import Test.Spec (describe)
import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)

import DOM.WebStorage.Storage (MOCK, STORAGE, newMockStorage)
import Spec.DOM.WebStorage.Generic as Generic
import Spec.DOM.WebStorage.JSON as JSON
import Spec.DOM.WebStorage.String as String
import Spec.DOM.WebStorage.Var as Var

main :: Eff (console :: CONSOLE, process :: PROCESS, storage :: STORAGE, mock :: MOCK) Unit
main = do
  storage <- newMockStorage
  run [ consoleReporter ] $ describe "DOM.WebStorage" do
    Generic.spec storage
    JSON.spec storage
    String.spec storage
    Var.spec storage
