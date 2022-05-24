{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Hydra.ContractModelTest where

import Control.Lens

import Hydra.Prelude as Prelude
import Hydra.Chain.Direct.Context
import Hydra.Chain.Direct.State

import GHC.Show (Show (..))
import Ledger (pubKeyHash)
import Ledger.Ada as Ada
import Ledger.Typed.Scripts (MintingPolicy)
import Plutus.Contract (Contract, Endpoint, type (.\/), awaitPromise, endpoint)
import Plutus.Contract.Test (Wallet (..))
import Plutus.Contract.Test.ContractModel
import Plutus.Contract.Types (ContractError)
import Plutus.Trace.Emulator (callEndpoint)
import Test.QuickCheck (Property, Testable (property))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Wallet.Emulator.Wallet (knownWallet)

w1, w2, w3 :: Wallet
[w1, w2, w3] = map knownWallet [1, 2, 3]

wallets :: [Wallet]
wallets = [w1, w2, w3]

data HeadState = Idle | Initialised | Committing | Opened | Closed
  deriving (Eq, Show)

data HydraModel = Uninitialized
                | HydraModel { headState    :: HeadState
                             , hydraContext :: HydraContext }
  deriving (Eq, Show)

type Schema = Endpoint "action" EndpointCall

data EndpointCall = EndpointInit
                  | EndpointCommit Ada.Ada
                  | EndpointCollectCom
                  | EndpointClose
  deriving (Generic, FromJSON, ToJSON)

instance HasActions HydraModel where
  getAllSymtokens _ = mempty

instance ContractModel HydraModel where
  data Action HydraModel
    = Setup HydraContext (OnChainHeadState 'StIdle)
    | Init Wallet
    | Commit Wallet Ada.Ada
    | CollectCom Wallet
    | Close Wallet
    deriving (Eq, Show)

  data ContractInstanceKey HydraModel w schema err params where
    HeadParty :: Wallet -> ContractInstanceKey HydraModel () Schema ContractError (OnChainHeadState 'StIdle)

  arbitraryAction s = case s ^. contractState of
    Uninitialized -> do
      ctx <- genHydraContextFor 3
      st  <- genStIdle ctx
      pure $ Setup ctx st
    HydraModel{} -> pure $ Init w1

  precondition ms = \ case
    Setup{}      -> s == Uninitialized
    Init{}       -> s /= Uninitialized
    Commit{}     -> s /= Uninitialized
    CollectCom{} -> s /= Uninitialized
    Close{}      -> s /= Uninitialized
    where
      s = ms ^. contractState

  initialState = Uninitialized

  nextState (Setup ctx _st) = put $ HydraModel Idle ctx
  nextState _ = pure ()

  initialInstances = []

  startInstances _ (Setup _ st) = [ StartContract (HeadParty w) st | w <- wallets ]
  startInstances _ _ = []

  instanceWallet (HeadParty w) = w

  instanceContract toks (HeadParty w) st = hydraContract st

  perform handle _ _ = \ case
    Init w -> do
      callEndpoint @"action" (handle $ HeadParty w) EndpointInit
    _ -> pure ()

deriving instance Eq (ContractInstanceKey HydraModel w schema err params)
deriving instance Show (ContractInstanceKey HydraModel w schema err params)

hydraContract :: OnChainHeadState 'StIdle -> Contract () Schema ContractError ()
hydraContract st = loop (SomeOnChainHeadState st)
  where
    loop st = awaitPromise (endpoint @"action" $ handleAction st) >>= loop

    handleAction st = \ case
      EndpointInit       -> pure st
      EndpointCommit ada -> pure st
      EndpointCollectCom -> pure st
      EndpointClose      -> pure st

prop_HydraOCV :: Actions HydraModel -> Property
prop_HydraOCV = propRunActions_

tests :: TestTree
tests =
  testGroup
    "Model Based Testing of Hydra Head"
    [testProperty "Hydra On-Chain Validation Protocol" $ property prop_HydraOCV]

