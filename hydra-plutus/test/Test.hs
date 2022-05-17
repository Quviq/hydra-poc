module Main where

import Hydra.Prelude

import qualified Hydra.ContractModelTest as Model
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Contracts tests"
    [ Model.tests
    ]
