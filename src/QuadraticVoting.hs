{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map as Map
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Constraints (TxConstraints)
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract (ToSchema, ensureKnownCurrencies, printJson, printSchemas, stage)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract
import PlutusTx (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins as Builtins
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Text.Printf (printf)
import Prelude (IO, Semigroup (..), Show, String, toInteger, (^))

-- User will start a fund and specify which fund it is.Then others can cast their vote in this fund.
-- Once the fund ends , projects in this fund can collect their grants.
data VotingDatum = VotingDatum
  { projectPubKey :: PaymentPubKeyHash,
    amount :: Integer,
    fund :: Integer,
    paymentPubKey :: PaymentPubKeyHash
  }
  deriving (Show)

PlutusTx.unstableMakeIsData ''VotingDatum

{-# INLINEABLE mkValidator #-}
mkValidator :: VotingDatum -> Integer -> ScriptContext -> Bool
mkValidator dat r ctx =
  traceIfFalse "beneficiary's signature missing" signedByBeneficiary
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = True

data Voting

instance Scripts.ValidatorTypes Voting where
  type DatumType Voting = VotingDatum
  type RedeemerType Voting = Integer

typedValidator :: Scripts.TypedValidator Voting
typedValidator =
  Scripts.mkTypedValidator @Voting
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @VotingDatum @Integer

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

data StartParams = StartParams
  { spMatchAmount :: !Integer,
    spRoundEnd :: !POSIXTime,
    spFund :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data VoteParams = VoteParams
  { vpAmount :: !Integer,
    vpFund :: !Integer,
    vpProjectPubKey :: !PaymentPubKeyHash
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CollectParams = CollectParams
  { cpFund :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type VoteSchema =
  Endpoint "start" StartParams
    .\/ Endpoint "vote" VoteParams
    .\/ Endpoint "collect" CollectParams

-- Below is the function to start a fund, it is under development
-- start :: String
-- start = ""

--  Function to cast their vote and the project they are voting for.
vote :: forall w s e. AsContractError e => VoteParams -> Contract w s e ()
vote vp = do
  pkh <- ownPaymentPubKeyHash
  previousUtxos <- Map.filter (isSuitable pkh (vpFund vp) (vpProjectPubKey vp)) <$> utxosAt scrAddress
  if (vpAmount vp) /= (((toInteger (Map.size previousUtxos) + 1) ^ 2) * 1000000000)
    then logInfo @String $ printf "not right amount to vote"
    else do
      let dat =
            VotingDatum
              { amount = vpAmount vp,
                projectPubKey = vpProjectPubKey vp,
                fund = vpFund vp,
                paymentPubKey = pkh
              }
          tx = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ vpAmount vp
      ledgerTx <- submitTxConstraints typedValidator tx
      void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
      logInfo @String $ printf "vote success "
  where
    isSuitable :: PaymentPubKeyHash -> Integer -> PaymentPubKeyHash -> ChainIndexTxOut -> Bool
    isSuitable pkh fundRound projectKey o = case _ciTxOutDatum o of
      Left _ -> False
      Right (Datum e) -> case PlutusTx.fromBuiltinData e of
        Nothing -> False
        Just d -> paymentPubKey d == pkh && fund d == fundRound && projectPubKey d == projectKey

-- Function for projects to collect their funds
collect :: forall w s e. AsContractError e => CollectParams -> Contract w s e ()
collect cp = do
  pkh <- ownPaymentPubKeyHash
  utxos <- Map.filter (isSuitable pkh) <$> utxosAt scrAddress
  if Map.null utxos
    then logInfo @String $ "no funds available"
    else do
      let orefs = fst <$> Map.toList utxos
          lookups =
            Constraints.unspentOutputs utxos
              <> Constraints.otherScript validator
          tx :: TxConstraints Void Void
          tx =
            mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs]
      ledgerTx <- submitTxConstraintsWith @Void lookups tx
      void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
      logInfo @String $ "collected funds"
  where
    isSuitable :: PaymentPubKeyHash -> ChainIndexTxOut -> Bool
    isSuitable pkh o = case _ciTxOutDatum o of
      Left _ -> False
      Right (Datum e) -> case PlutusTx.fromBuiltinData e of
        Nothing -> False
        Just d -> projectPubKey d == pkh

endpoints :: Contract () VoteSchema Text ()
endpoints = awaitPromise (vote' `select` collect') >> endpoints
  where
    vote' = endpoint @"vote" vote
    collect' = endpoint @"collect" collect

mkSchemaDefinitions ''VoteSchema

mkKnownCurrencies []
