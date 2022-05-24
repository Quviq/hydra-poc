{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Hydra.ContractModelTest where

import Control.Lens

import Hydra.Prelude as Prelude
import Hydra.Chain.Direct.Context
import Hydra.Chain.Direct.State
import Hydra.Cardano.Api

import GHC.Show (Show (..))
import Cardano.Api
import Ledger (pubKeyHash)
import qualified Ledger
import qualified PlutusTx
import Ledger.Ada as Ada
import Ledger.Typed.Scripts (MintingPolicy)
import Plutus.Contract (Contract, Endpoint, awaitPromise, endpoint, submitBalancedTx)
import Plutus.Contract.Test (Wallet (..))
import Plutus.Contract.Test.ContractModel
import Plutus.Contract.Types (ContractError)
import Plutus.Contract.Wallet
import Plutus.Contract.CardanoAPI (fromCardanoTx)
import Plutus.ChainIndex.Tx
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
    HeadParty :: Wallet -> ContractInstanceKey HydraModel () Schema ContractError (HydraContext, OnChainHeadState 'StIdle)

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

  startInstances _ (Setup ctx st) = [ StartContract (HeadParty w) (ctx, st) | w <- wallets ]
  startInstances _ _ = []

  instanceWallet (HeadParty w) = w

  instanceContract toks (HeadParty w) st = hydraContract st

  perform handle _ _ = \ case
    Init w -> do
      callEndpoint @"action" (handle $ HeadParty w) EndpointInit
    _ -> pure ()

deriving instance Eq (ContractInstanceKey HydraModel w schema err params)
deriving instance Show (ContractInstanceKey HydraModel w schema err params)

-- -- This is not the right thing to do!
-- toEmulatorTx :: Ledger.SomeCardanoApiTx -> Ledger.Tx
-- toEmulatorTx (Ledger.SomeTx tx era) = fromChainTx $ either (error . Prelude.show) id $ fromCardanoTx era tx
--   where
--     fromChainTx tx = Ledger.Tx
--       { Ledger.txInputs      = tx ^. citxInputs
--       , Ledger.txCollateral  = error "todo"
--       , Ledger.txOutputs     = tx ^. citxOutputs
--       , Ledger.txMint        = mempty
--       , Ledger.txFee         = mempty
--       , Ledger.txValidRange  = tx ^. citxValidRange
--       , Ledger.txMintScripts = mempty
--       , Ledger.txSignatures  = error "todo"
--       , Ledger.txRedeemers   = tx ^. citxRedeemers
--       , Ledger.txData        = tx ^. citxData
--       }
--     -- _citxTxId       :: TxId,
--     -- -- ^ The id of this transaction.
--     -- _citxInputs     :: Set TxIn,
--     -- -- ^ The inputs to this transaction.
--     -- _citxOutputs    :: ChainIndexTxOutputs,
--     -- -- ^ The outputs of this transaction, ordered so they can be referenced by index.
--     -- _citxValidRange :: !SlotRange,
--     -- -- ^ The 'SlotRange' during which this transaction may be validated.
--     -- _citxData       :: Map DatumHash Datum,
--     -- -- ^ Datum objects recorded on this transaction.
--     -- _citxRedeemers  :: Map RedeemerHash Redeemer,
--     -- -- ^ Redeemers of the minting scripts.
--     -- _citxScripts    :: Map ScriptHash Script,
--     -- -- ^ The scripts (validator, stake validator or minting) part of cardano tx.
--     -- _citxCardanoTx  :: Maybe SomeCardanoApiTx

hydraContract :: (HydraContext, OnChainHeadState 'StIdle) -> Contract () Schema ContractError ()
hydraContract (ctx, st) = loop (SomeOnChainHeadState st)
  where
    loop st = awaitPromise (endpoint @"action" $ handleAction st) >>= loop

    handleAction (SomeOnChainHeadState st@(reifyState -> TkIdle)) EndpointInit = do
        seedInput <- fromPlutusTxOutRef <$> getUnspentOutput
        let tx = initialize (ctxHeadParameters ctx)
                            (ctxVerificationKeys ctx)
                            seedInput
                            st
            Just (_, st') = observeTx @_ @StInitialized tx st
        _ <- submitBalancedTx $ Ledger.CardanoApiTx $ Ledger.SomeTx tx AlonzoEraInCardanoMode
        return (SomeOnChainHeadState st')
    handleAction st _ = pure st

prop_HydraOCV :: Actions HydraModel -> Property
prop_HydraOCV = propRunActions_

tests :: TestTree
tests =
  testGroup
    "Model Based Testing of Hydra Head"
    [testProperty "Hydra On-Chain Validation Protocol" $ property prop_HydraOCV]

