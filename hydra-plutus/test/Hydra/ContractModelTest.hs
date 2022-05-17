{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Hydra.ContractModelTest where

import Hydra.Prelude as Prelude

import Data.Data
import GHC.Show (Show (..))
import Ledger (pubKeyHash)
import Ledger.Ada as Ada
import Ledger.Typed.Scripts (MintingPolicy)
import Plutus.Contract (Contract, Endpoint, type (.\/))
import Plutus.Contract.Test (Wallet (..))
import Plutus.Contract.Test.ContractModel
import Plutus.Contract.Types (ContractError)
import Test.QuickCheck (Property, Testable (property))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Wallet.Emulator.Wallet (knownWallet)

w1, w2, w3 :: Wallet
[w1, w2, w3] = map knownWallet [1, 2, 3]

wallets :: [Wallet]
wallets = [w1, w2, w3]

data HeadState = Initialised | Committing | Opened | Closed
  deriving (Eq, Show, Data)

data HydraModel = HydraModel {headState :: Maybe HeadState}
  deriving (Eq, Show, Data)

type Schema = Endpoint "todo" ()

deriving instance Data Ada.Ada

instance ContractModel HydraModel where
  data Action HydraModel
    = Init Wallet
    | Commit Wallet Ada.Ada
    | CollectCom Wallet
    | Close Wallet
    deriving (Eq, Show, Data)

  data ContractInstanceKey HydraModel w schema err params where
    HeadParty :: Wallet -> ContractInstanceKey HydraModel () Schema ContractError ()

  arbitraryAction _ = pure (Init w1)
  initialState = HydraModel Nothing
  nextState _ = pure ()

  -- Defines all the contract instances that will run as part of a test
  initialInstances = [StartContract (HeadParty w) () | w <- wallets]

  instanceWallet (HeadParty w) = w

  instanceContract toks (HeadParty w) () = hydraContract
    where
      hydraContract :: Contract () Schema ContractError ()
      hydraContract = return ()

  perform _ _ _ _ = return ()

deriving instance Eq (ContractInstanceKey HydraModel w schema err params)
deriving instance Show (ContractInstanceKey HydraModel w schema err params)

prop_HydraOCV :: Actions HydraModel -> Property
prop_HydraOCV = propRunActions_

tests :: TestTree
tests =
  testGroup
    "Model Based Testing of Hydra Head"
    [testProperty "Hydra On-Chain Validation Protocol" $ property prop_HydraOCV]

