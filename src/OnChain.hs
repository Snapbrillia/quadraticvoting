-- EXTENSIONS
-- {{{
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- }}}


-- MODULE
-- {{{
module OnChain where
-- }}}


-- IMPORTS
-- {{{
import           Control.Monad               hiding (fmap)
import           Data.Aeson                  (ToJSON, FromJSON)
import           Data.Text                   (Text)
import           GHC.Generics
import           Ledger
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Ada                  as Ada
import           Plutus.Contract
import           Plutus.V1.Ledger.Credential (Credential (..))
import           Plutus.V1.Ledger.Value      (valueOf, flattenValue)
import           Plutus.V1.Ledger.Scripts    (ValidatorHash (..))
import           PlutusTx                    (Data (..))
import qualified PlutusTx
import           PlutusTx.AssocMap           (Map)
import qualified PlutusTx.AssocMap           as Map
import qualified PlutusTx.Builtins           as Builtins
import           PlutusTx.Builtins.Internal  (BuiltinInteger)
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           PlutusTx.Prelude            (BuiltinByteString, (<>))
import           PlutusTx.Sqrt               (Sqrt (..), isqrt)
import qualified Prelude                     as P
import           Prelude                     (Show (..), String)
import           Schema                      (ToSchema)
-- }}}


-- DATUM
-- {{{
-- DONATIONS
-- {{{
data Donations = Donations
  { getDonations :: Map PaymentPubKeyHash Integer
  }

-- CAUTION: Not sure if Plutus's Map supports auto-sorting.
instance Eq Donations where
  {-# INLINABLE (==) #-}
  Donations ds0 == Donations ds1 = ds0 == ds1

PlutusTx.unstableMakeIsData ''Donations
-- }}}


-- PROJECT
-- {{{
{-# INLINABLE addressToBuiltinByteString #-}
addressToBuiltinByteString :: Address -> BuiltinByteString
addressToBuiltinByteString Address {..} =
  -- {{{
  case addressCredential of
    PubKeyCredential (PubKeyHash bbs) ->
      bbs
    ScriptCredential (ValidatorHash bbs) ->
      bbs
  -- }}}


data Project = Project
  { pAddress   :: !Address
  , pLabel     :: !BuiltinByteString
  , pRequested :: !Integer
  , pDonations :: !Donations
  }

instance Eq Project where
  {-# INLINABLE (==) #-}
  a == b = (pAddress a == pAddress b)
  -- It suffices to compare the addresses.

PlutusTx.unstableMakeIsData ''Project
-- }}}


-- QVF DATUM
-- {{{
-- `newtype` is worse than data for Plutus.
data QVFDatum = QVFDatum
  { qvfProjects :: ![Project]
  }


{-# INLINABLE sortProjects #-}
sortProjects :: [Project] -> [Project]
sortProjects =
  sortBy
    ( \p0 p1 ->
        compare
          (addressToBuiltinByteString $ pAddress p0)
          (addressToBuiltinByteString $ pAddress p1)
    )

instance Eq QVFDatum where
  {-# INLINABLE (==) #-}
  QVFDatum projs0 == QVFDatum projs1 =
    let
      go res []       []       = res
      go res []       _        = False
      go res _        []       = False
      -- Plutus is not lazy and (&&) would be inefficient.
      go res (x : xs) (y : ys) =
        if res then
          go (x == y) xs ys
        else
          False
    in
    go True (sortProjects projs0) (sortProjects projs1)

PlutusTx.unstableMakeIsData ''QVFDatum

initialDatum = QVFDatum []
-- }}}
-- }}}


-- REDEEMER
-- {{{
-- ADD PROJECT PARAMETERS
-- {{{
-- | A different datatype than `Project` to disallow
--   addition of a project with pre-filled donations.
data AddProjectParams = AddProjectParams
  { appAddress   :: !Address
  , appLabel     :: !BuiltinByteString
  , appRequested :: !Integer
  }

PlutusTx.unstableMakeIsData ''AddProjectParams
-- }}}


-- DONATE PARAMETERS
-- {{{
data DonateParams = DonateParams
  { dpDonor   :: !PaymentPubKeyHash
  , dpProject :: !Address
  , dpAmount  :: !Integer
  }

PlutusTx.unstableMakeIsData ''DonateParams
-- }}}


-- QVF ACTION
-- {{{
data QVFAction
  = AddProject AddProjectParams -- ^ Add projects that can be funded.
  | Donate     DonateParams     -- ^ Attempt donation to a project (identified simply by its address).
  | Distribute                  -- ^ Distribute the fund to projects per QVF
 

PlutusTx.makeIsDataIndexed ''QVFAction
  [ ('AddProject, 0)
  , ('Donate    , 1)
  , ('Distribute, 2)
  ]
-- }}}
-- }}}


-- QVF VALIDATOR 
-- {{{
{-# INLINABLE mkQVFValidator #-}
mkQVFValidator :: PaymentPubKeyHash
               -> QVFDatum
               -> QVFAction
               -> ScriptContext
               -> Bool
mkQVFValidator keyHolder currDatum action ctx =
  -- {{{
  let
    info = scriptContextTxInfo ctx

    -- | UTxO sitting at this script address.
    ownInput :: TxOut
    ownInput =
      -- {{{
      case findOwnInput ctx of
        Nothing ->
          traceError "Expected an input from own address."
        Just i  ->
          txInInfoResolved i
      -- }}}

    -- | UTxO being sent to this script address.
    ownOutput :: TxOut
    ownOutput =
      -- {{{
      case getContinuingOutputs ctx of
        [o] ->
          o
        _   ->
          traceError "Expected exactly one output to own address."
      -- }}}

    -- | Possible attached datum to the input UTxO.
    mInputDatum :: Maybe QVFDatum
    mInputDatum =
      -- {{{
      getDatumFromUTxO ownInput (`findDatum` info)
      -- }}}

    -- | Possible attached datum to the output UTxO
    --   (not properly implemented).
    mOutputDatum :: Maybe QVFDatum
    mOutputDatum =
      -- {{{
      getDatumFromUTxO ownOutput (`findDatum` info)
      -- }}}

    -- | Checks if the attached input datum matches the
    --   one provided to the script.
    validInputDatum :: Bool
    validInputDatum =
      -- {{{
      case mInputDatum of
        Nothing ->
          False
        Just inputDatum ->
          inputDatum == currDatum
      -- }}}

    -- | Checks if the attached output datum is
    --   properly updated.
    isOutputDatumValid :: QVFDatum -> Bool
    isOutputDatumValid newDatum =
      -- {{{
      case mOutputDatum of
        Nothing ->
          False
        Just outputDatum ->
          outputDatum == newDatum
      -- }}}

    -- | Helper function to see if @ownOutput@
    --   carries enough Lovelaces.
    enoughAdaInOutput :: Integer -> Bool
    enoughAdaInOutput addedAmount =
      -- {{{
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
      outVal == (inVal <> Ada.lovelaceValueOf addedAmount)
      -- }}}

    -- | Should check whether the distribution follows
    --   the quadratic formula (not implemented yet).
    correctlyDistributed = True -- TODO
  in
  traceIfFalse "Invalid input datum hash." validInputDatum &&
  case action of
    AddProject addProjectParams ->
      -- {{{
      let
        newProject   = initiateProject addProjectParams
        currProjects = qvfProjects currDatum
        projectIsNew = notElem newProject currProjects
        newDatum     = QVFDatum $ newProject : currProjects
      in
         traceIfFalse "Project submission fees not paid." (enoughAdaInOutput minLovelace)
      && traceIfFalse "Project already exists." projectIsNew
      && traceIfFalse "Invalid output datum hash." (isOutputDatumValid newDatum)
      -- }}}
    Donate     DonateParams     {..} ->
      -- {{{
      let
        currProjects                = qvfProjects currDatum
        go :: ([Project], Maybe Project, [Project]) -> ([Project], Maybe Project, [Project])
        go acc@([], Nothing, _)     = acc
        go acc@(_, Just _, _)       = acc
        go (p : ps, Nothing, acc)
          -- {{{
          | pAddress p == dpProject =
              ( ps
              , Just $ addDonationToProject dpDonor dpAmount p
              , acc
              )
          | otherwise =
              go (ps, Nothing, p : acc)
          -- }}}
        newDatum =
          -- {{{
          case go (currProjects, Nothing, []) of
            (_, Nothing, _) ->
              traceError "Target project not found."
            (leftPs, Just updatedP, rightPs) ->
              QVFDatum $ leftPs ++ (updatedP : rightPs)
          -- }}}
      in
         traceIfFalse "Donation less than minimum."    (dpAmount < minLovelace)
      && traceIfFalse "Not enough Lovelaces provided." (enoughAdaInOutput dpAmount)
      && traceIfFalse "Invalid output datum hash."     (isOutputDatumValid newDatum)
      -- }}}
    Distribute                       ->
      -- {{{
      let
        correctlyDistributed = True -- TODO
      in
         traceIfFalse "Unauthorized." (txSignedBy info $ unPaymentPubKeyHash keyHolder)
      && traceIfFalse "Improper distribution." correctlyDistributed
      -- }}}
  -- }}}


-- TEMPLATE HASKELL, BOILERPLATE, ETC. 
-- {{{
data QVF
instance Scripts.ValidatorTypes QVF where
    type DatumType    QVF = QVFDatum
    type RedeemerType QVF = QVFAction


typedQVFValidator :: PaymentPubKeyHash -> Scripts.TypedValidator QVF
typedQVFValidator keyHolder =
  -- {{{
  Scripts.mkTypedValidator @QVF
    ( PlutusTx.applyCode
        $$(PlutusTx.compile [|| mkQVFValidator ||])
        (PlutusTx.liftCode keyHolder)
    )
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @QVFDatum @QVFAction
  -- }}}


qvfValidator :: PaymentPubKeyHash -> Validator
qvfValidator = Scripts.validatorScript . typedQVFValidator


qvfValidatorHash :: PaymentPubKeyHash -> ValidatorHash
qvfValidatorHash = Scripts.validatorHash . typedQVFValidator


qvfAddress :: PaymentPubKeyHash -> Address
qvfAddress = scriptAddress . qvfValidator
-- }}}
-- }}}


-- UTILS 
-- {{{
{-# INLINABLE initiateProject #-}
initiateProject :: AddProjectParams -> Project
initiateProject AddProjectParams {..} =
  -- {{{
  Project
    { pAddress   = appAddress
    , pLabel     = appLabel
    , pRequested = appRequested
    , pDonations = Donations Map.empty
    }
  -- }}}


{-# INLINABLE addDonation #-}
addDonation :: PaymentPubKeyHash -> Integer -> Donations -> Donations
addDonation donor lovelaces (Donations kvs) =
  -- {{{
  Donations $ Map.unionWith (+) kvs $ Map.singleton donor lovelaces
  -- }}}


{-# INLINABLE addDonationToProject #-}
addDonationToProject :: PaymentPubKeyHash -> Integer -> Project -> Project
addDonationToProject donor lovelaces proj =
  -- {{{
  proj
    { pDonations =
        addDonation donor lovelaces (pDonations proj)
    }
  -- }}}


{-# INLINABLE getDatumFromUTxO #-}
getDatumFromUTxO :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe QVFDatum
getDatumFromUTxO TxOut {..} converter = do
  -- {{{
  datumHash <- txOutDatumHash
  Datum d   <- converter datumHash
  PlutusTx.fromBuiltinData d
  -- }}}
-- }}}


-- CONSTANTS
-- {{{
minLovelace :: Integer
minLovelace = 2000000
-- }}}


-- TxInfo = TxInfo
--   { txInfoInputs      :: [TxInInfo]
--   , txInfoOutputs     :: [TxOut]
--   , txInfoFee         :: Value
--   , txInfoMint        :: Value
--   , txInfoDCert       :: [DCert]
--   , txInfoWdrl        :: [(StakingCredential, Integer)]
--   , txInfoValidRange  :: POSIXTimeRange
--   , txInfoSignatories :: [PubKeyHash]
--   , txInfoData        :: [(DatumHash, Datum)]
--   , txInfoId          :: TxId
--   }

-- TxInInfo = TxInInfo
--   { txInInfoOutRef    :: TxOutRef
--   , txInInfoResolved  :: TxOut
--   }

-- TxOut = TxOut
--   { txOutAddress      :: Address
--   , txOutValue        :: Value
--   , txOutDatumHash    :: Maybe DatumHash
--   }


