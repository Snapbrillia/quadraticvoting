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
{-# LANGUAGE TupleSections         #-}
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
module QVF where
-- }}}


-- IMPORTS
-- {{{
import           Ledger
import qualified Ledger.Typed.Scripts        as Scripts
import qualified Ledger.Ada                  as Ada
import qualified Plutonomy
import           Plutus.Contract
import           Plutus.V1.Ledger.Credential (Credential (..))
import qualified Plutus.V1.Ledger.Interval   as Interval
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Scripts    (ValidatorHash (..))
import           PlutusTx                    (Data (..))
import qualified PlutusTx
import qualified PlutusTx.AssocMap           as Map
import           PlutusTx.AssocMap           (Map)
import qualified PlutusTx.Builtins           as Builtins
import           PlutusTx.Prelude            hiding (unless)
import           PlutusTx.Prelude            (BuiltinByteString, (<>))
import           PlutusTx.Sqrt               (Sqrt (..), isqrt)
import           Prelude                     (Show, show)
import qualified Prelude                     as P
import           Utils
import qualified Minter.NFT                  as NFT
-- }}}


-- QVF PARAMETERS
-- {{{
data QVFParams = QVFParams
  { qvfKeyHolder      :: !PubKeyHash
  , qvfSymbol         :: !CurrencySymbol
  , qvfProjectSymbol  :: !CurrencySymbol
  , qvfDonationSymbol :: !CurrencySymbol
  , qvfDeadline       :: !POSIXTime
  }

PlutusTx.makeLift ''QVFParams


{-# INLINABLE qvfAsset #-}
qvfAsset :: QVFParams -> AssetClass
qvfAsset qvf = AssetClass (qvfSymbol qvf, NFT.qvfTokenName)
-- }}}


-- QVF DATUM
-- {{{
-- PROJECT DETAILS
-- {{{
data ProjectDetails = ProjectDetails
  { pdId         :: !BuiltinByteString
  , pdPubKeyHash :: !PubKeyHash
  , pdName       :: !BuiltinByteString
  , pdRequested  :: !Integer
  }

PlutusTx.unstableMakeIsData ''ProjectDetails
-- }}}


-- DATUM
-- {{{
data QVFDatum
  -- {{{
  = RegisteredProjectsCount 
    -- ^ The "main" datum, keeping a record of the number of registered projects.
      -- {{{
      !Integer
      -- }}}

  | DonationAccumulationProgress
    -- ^ For keeping track of the folded projects traversed.
      -- {{{
      !Integer -- ^ Total project count.
      !Integer -- ^ Projects traversed so far.
      !Integer -- ^ Total Lovelaces so far.
      !Integer -- ^ Sum of prize weights so far.
      -- }}}

  | DonationAccumulationConcluded
    -- ^ Datum after collecting all donations.
      -- {{{
      !Integer -- ^ Total project count.
      !Integer -- ^ Total Lovelaces.
      !Integer -- ^ Sum of prize weights.
      !Bool    -- ^ Key holder fee collected or not.
      -- }}}

  | ProjectInfo
    -- ^ To store static info in a reference UTxO.
      -- {{{
      ProjectDetails
      -- }}}

  | ReceivedDonationsCount
    -- ^ Datum for a project UTxO. Tracks number of donations, not amount.
      -- {{{
      !Integer
      -- }}}
  
  | DonationFoldingProgress
    -- ^ Project UTxO during the first phase of folding the donations.
      -- {{{
      !Integer -- ^ Total donation count.
      !Integer -- ^ Folded so far.
      -- }}}
  
  | DonationFoldingConcluded
    -- ^ Project UTxO after the last folding transaction of phase one.
      -- {{{
      !Integer -- ^ Total donation count.
      -- }}}

  | PrizeWeight
    -- ^ Result of folding all donations.
      -- {{{
      !Integer -- ^ Prize weight.
      !Bool    -- ^ Whether depleted or not.
      -- }}}

  | Donation
    -- ^ For a single donation UTxO.
      -- {{{
      !PubKeyHash -- ^ Donor's public key hash.
      -- }}}

  | Donations
    -- ^ For output donation UTxO of the first phase of folding donations.
      -- {{{
      !(Map PubKeyHash Integer)
      -- }}}

  | Escrow
    -- ^ For UTxOs that store the excess reward won by projects.
      -- {{{
      !(Map BuiltinByteString Integer)
      -- }}}
  -- }}}

PlutusTx.makeIsDataIndexed ''QVFDatum
  [ ('RegisteredProjectsCount      , 0)
  , ('DonationAccumulationProgress , 1)
  , ('DonationAccumulationConcluded, 2)
  , ('ProjectInfo                  , 3)
  , ('ReceivedDonationsCount       , 4)
  , ('DonationFoldingProgress      , 5)
  , ('DonationFoldingConcluded     , 6)
  , ('PrizeWeight                  , 7)
  , ('Donation                     , 8)
  , ('Donations                    , 9)
  , ('Escrow                       , 10)
  ]
-- }}}
-- }}}


-- QVF ACTION
-- {{{
-- REGISTRATION INFO
-- {{{
data RegistrationInfo = RegistrationInfo
  { riTxOutRef   :: !TxOutRef
  , riPubKeyHash :: !BuiltinByteString
  , riLabel      :: !BuiltinByteString
  , riRequested  :: !Integer
  }

PlutusTx.unstableMakeIsData ''RegistrationInfo
-- }}}


-- DONATION INFO
-- {{{
data DonationInfo = DonationInfo
  { diProjectId :: !BuiltinByteString
  , diDonor     :: !BuiltinByteString
  , diAmount    :: !Integer
  }

PlutusTx.unstableMakeIsData ''DonationInfo
-- }}}


-- REDEEMER
-- {{{
data QVFAction
  = RegisterProject       RegistrationInfo
  | DonateToProject       DonationInfo
  | FoldDonationsPhaseOne
  | FoldDonationsPhaseTwo
  | AccumulateDonations
  | PayKeyHolderFee
  | DistributePrize
  | WithdrawBounty

PlutusTx.makeIsDataIndexed ''QVFAction
  [ ('RegisterProject      , 0)
  , ('DonateToProject      , 1)
  , ('FoldDonationsPhaseOne, 2)
  , ('FoldDonationsPhaseTwo, 3)
  , ('AccumulateDonations  , 4)
  , ('PayKeyHolderFee      , 5)
  , ('DistributePrize      , 6)
  , ('WithdrawBounty       , 7)
  ]
-- }}}
-- }}}


-- QVF VALIDATOR 
-- {{{
{-# INLINABLE mkQVFValidator #-}
mkQVFValidator :: QVFParams
               -> QVFDatum
               -> QVFAction
               -> ScriptContext
               -> Bool
mkQVFValidator qvfParams datum action ctx =
  -- {{{
  let
    info = scriptContextTxInfo ctx
  in
  case (datum, action) of
    (RegisteredProjectsCount       soFar          , RegisterProject regInfo) -> traceError "TODO."
    (ReceivedDonationsCount        soFar          , DonateToProject donInfo) -> traceError "TODO."
    (Donation                      donorsPKH      , FoldDonationsPhaseOne  ) -> traceError "TODO."
    (ReceivedDonationsCount        soFar          , FoldDonationsPhaseOne  ) -> traceError "TODO."
    (DonationFoldingProgress       tot soFar      , FoldDonationsPhaseOne  ) -> traceError "TODO."
    (Donations                     pkhToAmountMap , FoldDonationsPhaseTwo  ) -> traceError "TODO."
    (DonationFoldingConcluded      tot            , FoldDonationsPhaseTwo  ) -> traceError "TODO."
    (DonationAccumulationProgress  tot ps ds w    , AccumulateDonations    ) -> traceError "TODO."
    (PrizeWeight                   weight False   , AccumulateDonations    ) -> traceError "TODO."
    (DonationAccumulationConcluded ps ds den False, PayKeyHolderFee        ) -> traceError "TODO."
    (DonationAccumulationConcluded ps ds den True , DistributePrize        ) -> traceError "TODO."
    (PrizeWeight                   weight True    , DistributePrize        ) -> traceError "TODO."
    (Escrow                        weight True    , WithdrawBounty         ) -> traceError "TODO."
    (_                                            , _                      ) ->
      traceError "Invalid transaction."
  -- }}}


-- TEMPLATE HASKELL, BOILERPLATE, ETC. 
-- {{{
data QVF
instance Scripts.ValidatorTypes QVF where
  type DatumType    QVF = QVFDatum
  type RedeemerType QVF = QVFAction


typedQVFValidator :: QVFParams -> Scripts.TypedValidator QVF
typedQVFValidator qvfParams =
  -- {{{
  Scripts.mkTypedValidator @QVF
    ( PlutusTx.applyCode
        $$(PlutusTx.compile [|| mkQVFValidator ||])
        (PlutusTx.liftCode qvfParams)
    )
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @QVFDatum @QVFAction
  -- }}}


qvfValidator :: QVFParams -> Validator
qvfValidator =
    Plutonomy.optimizeUPLC
  . Scripts.validatorScript
  . typedQVFValidator


qvfValidatorHash :: QVFParams -> ValidatorHash
qvfValidatorHash = Scripts.validatorHash . typedQVFValidator


qvfAddress :: QVFParams -> Address
qvfAddress = scriptAddress . qvfValidator
-- }}}
-- }}}
