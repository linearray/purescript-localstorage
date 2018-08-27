module Test.Main where

import Prelude

import Effect                           (Effect)
import Test.Spec                        (describe)
import Test.Spec.Runner                 (run)
import Test.Spec.Reporter.Console       (consoleReporter)

import DOM.WebStorage.ForeignStorage    (newMockStorage)
import Spec.DOM.WebStorage.Generic      as Generic
import Spec.DOM.WebStorage.JSON         as JSON
import Spec.DOM.WebStorage.String       as String
--import Spec.DOM.WebStorage.Var        as Var

main :: Effect Unit
main = do
  storage <- newMockStorage
  run [ consoleReporter ] $ describe "DOM.WebStorage" do
    Generic.spec    storage
    JSON.spec       storage
    String.spec     storage
--    Var.spec storage
