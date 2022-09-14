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
  , qvfTokenName      :: !TokenName
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
  { pdPubKeyHash :: !PubKeyHash
  , pdName       :: !BuiltinByteString
  , pdRequested  :: !Integer
  }
instance Eq QVFDatum where
  {-# INLINABLE (==) #-}
  ProjectDetails p0 n0 r0 == ProjectDetails p1 n1 r1 =
    -- {{{
    p0 == p1 && n0 == n1 && r0 == r1
    -- }}}

PlutusTx.unstableMakeIsData ''ProjectDetails
-- }}}


-- DATUM
-- {{{
data QVFDatum
  -- {{{
  = DeadlineDatum
    -- ^ The datum attached to a reference UTxO for reading the deadline of the fund.
      -- {{{
      !POSIXTime
      -- }}}

  | RegisteredProjectsCount 
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

instance Eq QVFDatum where
  {-# INLINABLE (==) #-}
  -- {{{
  DeadlineDatum pt0 == DeadlineDatum pt1 = pt0 == pt1
  RegisteredProjectsCount c0 == RegisteredProjectsCount c1 = c0  == c1
  DonationAccumulationProgress t0 s0 d0 w0 == DonationAccumulationProgress t1 s1 d1 w1 = t0 == t1 && s0 == s1 && d0 == d1 && w0 == w1
  DonationAccumulationConcluded t0 d0 w0 k0 == DonationAccumulationConcluded t1 d1 w1 k1 = t0 == t1 && d0 == d1 && w0 == w1 && k0 == k1
  ProjectInfo dets0 == ProjectInfo dets1 = dets0 == dets1
  ReceivedDonationsCount c0 == ReceivedDonationsCount c1 = c0 == c1
  DonationFoldingProgress t0 s0 == DonationFoldingProgress t1 s1 = t0 == t1 && s0 == s1
  DonationFoldingConcluded t0 == DonationFoldingConcluded t1 = t0 == t1
  PrizeWeight w0 d0 == PrizeWeight w1 d1 = w0 == w1 && d0 == d1
  Donation p0 == Donation p1 = p0 == p1
  Donations m0 == Donations m1 = m0 == m1
  Escrow m0 == Escrow m1 = m0 == m1
  _ == _ = False
  -- }}}

PlutusTx.makeIsDataIndexed ''QVFDatum
  [ ('DeadlineDatum                , 0)
  , ('RegisteredProjectsCount      , 1)
  , ('DonationAccumulationProgress , 2)
  , ('DonationAccumulationConcluded, 3)
  , ('ProjectInfo                  , 4)
  , ('ReceivedDonationsCount       , 5)
  , ('DonationFoldingProgress      , 6)
  , ('DonationFoldingConcluded     , 7)
  , ('PrizeWeight                  , 8)
  , ('Donation                     , 9)
  , ('Donations                    , 10)
  , ('Escrow                       , 11)
  ]
-- }}}
-- }}}


-- QVF ACTION
-- {{{
-- REGISTRATION INFO
-- {{{
data RegistrationInfo = RegistrationInfo
  { riTxOutRef   :: !TxOutRef
  , riPubKeyHash :: !PubKeyHash
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
  = UpdateDeadline        !POSIXTime
  | RegisterProject       RegistrationInfo
  | DonateToProject       DonationInfo
  | FoldDonationsPhaseOne
  | FoldDonationsPhaseTwo
  | AccumulateDonations
  | PayKeyHolderFee
  | DistributePrize
  | WithdrawBounty

PlutusTx.makeIsDataIndexed ''QVFAction
  [ ('UpdateDeadline       , 0)
  , ('RegisterProject      , 1)
  , ('DonateToProject      , 2)
  , ('FoldDonationsPhaseOne, 3)
  , ('FoldDonationsPhaseTwo, 4)
  , ('AccumulateDonations  , 5)
  , ('PayKeyHolderFee      , 6)
  , ('DistributePrize      , 7)
  , ('WithdrawBounty       , 8)
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
mkQVFValidator QVFParams{..} datum action ctx =
  -- {{{
  let
    info = scriptContextTxInfo ctx

    -- | The UTxO currently being validated.
    currUTxO :: TxOut
    currUTxO =
      -- {{{
      case findOwnInput ctx of
        Nothing ->
          traceError "Couldn't find UTxO."
        Just i  ->
          txInInfoResolved i
      -- }}}

    -- | Script's address.
    ownAddr :: Address
    ownAddr =
      -- {{{
      txOutAddress currUTxO
      -- }}}

    -- | Extracts the inline datum from a given UTxO.
    --
    -- Raises exception upon failure.
    getInlineDatum :: TxOut -> QVFDatum
    getInlineDatum utxo =
      -- {{{
      case txOutDatum utxo of
        OutputDatum (Datum d) ->
          d
        _                     ->
          traceError "Bad inline datum."
      -- }}}

    -- | Checks if a given UTxO is in fact from this contract.
    utxoSitsAtScript :: TxOut -> Bool
    utxoSitsAtScript =
      -- {{{
      (== ownAddr) . txOutAddress
      -- }}}

    -- | Checks for key holder's signature. Induced laziness.
    signedByKeyHolder :: () -> Bool
    signedByKeyHolder _ =
      -- {{{
      traceIfFalse "Unauthorized." $ txSignedBy info qvfKeyHolder
      -- }}}

    -- | Checks if a given UTxO has exactly 1 of asset X.
    utxoHasX :: CurrencySymbol -> TokenName -> TxOut -> Bool
    utxoHasX sym tn utxo =
      -- {{{
        txOutValue utxo
      & flattenValue
      & find (\(sym', tn', amt') -> sym' == sym && tn' == tn && amt' == 1)
      & isJust
      -- }}}

    -- | Checks if the UTxO currently being validated carries a single X asset.
    currUTxOHasX :: CurrencySymbol -> TokenName -> Bool
    currUTxOHasX sym tn =
      -- {{{
      utxoHasX sym tn currUTxO
      -- }}}

    -- | Tries to find an input from the script that carries a single X asset.
    --
    --   Expects to find exactly 1. Raises exception otherwise.
    getXInputUTxO :: CurrencySymbol -> TokenName -> TxOut
    getXInputUTxO sym tn =
      -- {{{
      case filter (utxoHasX sym tn . txInInfoResolved) (txInfoInputs info) of
        [txIn] ->
          let
            inUTxO = txInInfoResolved txIn
          in
          if utxoSitsAtScript inUTxO then
            inUTxO
          else
            traceError "Input authentication asset must come from the script."
        _      ->
          traceError "There should be exactly 1 authentication asset in input."
      -- }}}

    -- | Looks inside the reference inputs and extracts the inline datum
    --   attached to one of which carries the given asset.
    --
    --   Raises exception if the reference input, or the datum is not found.
    getDatumFromRefX :: CurrencySymbol -> TokenName -> QVFDatum
    getDatumFromRefX sym tn =
      -- {{{
      case find (utxoHasX sym tn . txInInfoResolved) (txInfoReferenceInputs info) of
        Just txIn ->
          -- {{{
          getInlineDatum (txInInfoResolved txIn)
          -- }}}
        Nothing   ->
          -- {{{
          traceError "Missing reference input."
          -- }}}
      -- }}}

    -- | Checks if a UTxO carries a specific inline datum.
    --
    -- Raises exception upon failure.
    utxosDatumMatchesWith :: QVFDatum -> TxOut -> Bool
    utxosDatumMatchesWith newDatum =
      -- {{{
      (newDatum ==) . getInlineDatum
      -- }}}

    -- | Looks for the presence of a UTxO in the input list with 1 X asset, and
    --   a datum that complies with the given predicate.
    xInputWithSpecificDatumExists :: CurrencySymbol
                                  -> TokenName
                                  -> (QVFDatum -> Bool)
                                  -> Bool
    xInputWithSpecificDatumExists sym tn datumPred =
      -- {{{
      let
        pred txIn =
          -- {{{
          let
            txOut = txInInfoResolved txIn
          in
             utxoHasX sym tn txOut
          && datumPred (getInlineDatum txOut)
          -- }}}
      in
      isJust $ find pred $ txInfoInputs info
      -- }}}
  
    -- | Checks 1 X comes from the script, and 1 X goes back to the script,
    --   with enough added Lovelaces and properly updated datum.
    --
    --   Raises exception on @False@.
    xIsPresent :: CurrencySymbol -- ^ X's currency symbol
               -> TokenName      -- ^ X's token name
               -> Integer        -- ^ Increase in output's Lovelace count
               -> QVFDatum       -- ^ Updated datum
               -> Bool
    xIsPresent sym tn increaseInLovelace newDatum =
      -- {{{
      let
        inUTxO = getXInputUTxO sym tn
      in
      case filter (utxoHasX sym tn) (getContinuingOutputs ctx) of
        [txOut] ->
          -- {{{
          if utxoSitsAtScript txOut then
            -- {{{
            let
              inVal         = txOutValue inUTxO
              outVal        = txOutValue txOut
              desiredOutVal =
                inVal <> Ada.lovelaceValueOf increaseInLovelace
            in
               traceIfFalse
                 "Authenticated output doesn't have enough Lovelaces"
                 (outVal == desiredOutVal)
            && traceIfFalse
                 "Invalid datum attached to the authenticated output."
                 (utxosDatumMatchesWith newDatum txOut)
            -- }}}
          else
            -- {{{
            traceError "Produced authentication asset must be sent back to the script."
            -- }}}
          -- }}}
        _       ->
          -- {{{
          traceError "There must be exactly 1 authentication asset produced."
          -- }}}
      -- }}}

    -- | Looks for the deadline reference UTxO, and checks if the funding round
    --   is still in progress.
    --
    --   Raises exception on @False@.
    canRegisterOrDonate :: () -> Bool
    canRegisterOrDonate _ =
      -- {{{
      case getDatumFromRefX qvfSymbol qvfTokenName of
        DeadlineDatum dl ->
          traceIfFalse "This funding round is over." $ deadlineNotReached dl
        _                ->
          traceError "Invalid deadline datum."
      -- }}}

    -- | Checks whether project registrations and donations are still allowed.
    deadlineNotReached :: POSIXTime -> Bool
    deadlineNotReached dl =
      -- {{{
      Interval.to dl `Interval.contains` txInfoValidRange info
      -- }}}

    -- | Checks whether the distribution can be triggered.
    deadlineReached :: POSIXTime -> Bool
    deadlineReached dl =
      -- {{{
      Interval.from dl `Interval.contains` txInfoValidRange info
      -- }}}

    -- | Validates the minting value.
    mintIsPresent :: CurrencySymbol -> TokenName -> Integer -> Bool
    mintIsPresent sym tn amt =
      -- {{{
      any
        (\(sym', tn', amt') -> sym' == sym && tn' == tn && amt' == amt)
        (flattenValue $ txInfoMint info)
      -- }}}

    -- | Helper function to see if the given UTxO carries the given amount of
    --   Lovelaces.
    utxoHasLovelaces :: Integer -> TxOut -> Bool
    utxoHasLovelaces lovelaces txOut =
      -- {{{
      txOutValue txOut == Ada.lovelaceValueOf lovelaces
      -- }}}
  in
  case (datum, action) of
    (DeadlineDatum currDl                         , UpdateDeadline newDl   ) ->
      -- {{{
         traceIfFalse
           "New deadline has already passed."
           (deadlineReached newDl)
      && traceIfFalse
           "Missing authentication asset."
           (currUTxOHasX qvfSymbol qvfTokenName)
      && ( case getContinuingOutputs ctx of
             [o] ->
               -- {{{
               traceIfFalse
                 "Invalid output datum."
                 (utxosDatumMatchesWith (DeadlineDatum newDl) o)
               -- }}}
             _   ->
               -- {{{
               traceError
                 "There should be exactly 1 UTxO going back to the script."
               -- }}}
         )
      -- }}}

    (RegisteredProjectsCount soFar                , RegisterProject regInfo) ->
      -- Project Registration
      -- {{{
      let
        tn = orefToTokenName $ riTxOutRef regInfo

        -- | Raises exception on @False@.
        outputPsArePresent :: Bool
        outputPsArePresent =
          -- {{{
          case filter (utxoHasX qvfProjectSymbol tn) getContinuingOutputs of
            -- TODO: Is it OK to expect a certain order for these outputs?
            --       This is desired to avoid higher transaction fees.
            [o0, o1] ->
              -- {{{
                 traceIfFalse
                   "First continuing output must carry project's static info."
                   ( utxosDatumMatchesWith
                       (ProjectInfo $ registrationInfoToProjectDetials regInfo)
                       o0
                   )
              && traceIfFalse
                   "Second continuing output must carry record of donations."
                   ( utxosDatumMatchesWith
                       (ReceivedDonationsCount 0)
                       o1
                   )
              && traceIfFalse
                   "Half of the registration fee should be stored in project's reference UTxO."
                   (utxoHasLovelaces halfOfTheRegistrationFee o0)
              && traceIfFalse
                   "Half of the registration fee should be stored in project's main UTxO."
                   (utxoHasLovelaces halfOfTheRegistrationFee o1)
              -- }}}
            _        ->
              -- {{{
              traceError "There should be exactly 2 project UTxOs produced."
              -- }}}
          -- }}}
      in
         traceIfFalse
           "There should be exactly 2 project assets minted."
           (mintIsPresent qvfProjectSymbol tn 2)
      && xIsPresent
           qvfSymbol
           qvfTokenName
           0
           (RegisteredProjectsCount $ soFar + 1)
      && canRegisterOrDonate ()
      && outputPsArePresent
      -- }}}

    (ReceivedDonationsCount soFar                 , DonateToProject donInfo) ->
      -- Project Donation
      -- {{{
      let
        tn = TokenName $ diProjectId donInfo

        -- | Raises exception on @False@.
        outputVIsPresent :: Bool
        outputVIsPresent =
          -- {{{
          case filter (utxoHasX qvfDonationSymbol tn) getContinuingOutputs of
            [o] ->
              -- {{{
                 traceIfFalse
                   "Produced donation UTxO must carry donor's public key hash as an inlinde datum."
                   ( utxosDatumMatchesWith
                       (Donation $ diDonor donInfo)
                       o
                   )
              && traceIfFalse
                   "Donation UTxO must carry exactly the same Lovelace count as specified."
                   (utxoHasLovelaces (diAmount donInfo) o)
              -- }}}
            _        ->
              -- {{{
              traceError "There should be exactly 1 donation UTxO produced."
              -- }}}
          -- }}}
      in
         traceIfFalse
           "There should be exactly 1 donation asset minted."
           (mintIsPresent qvfDonationSymbol tn 1)
      && xIsPresent qvfProjectSymbol tn 0 (ReceivedDonationsCount $ soFar + 1)
      && canRegisterOrDonate ()
      && outputVIsPresent
      -- }}}

    (Donation donorsPKH                           , FoldDonationsPhaseOne  ) ->
      -- First Phase of Folding Donations
      -- To avoid excessive transaction fees, this endpoint delegates its
      -- logic to the @P@ UTxO by checking that it is in fact being spent.
      -- {{{ 
      let
        -- | Finds the token name of the current donation UTxO being spent.
        --   Used to check the presence of relevant project UTxO.
        --
        --   Raises exception upon failure.
        tn =
          -- {{{
          case flattenValue (txOutValue currUTxO) of
            [_, (sym', tn', amt')] ->
            -- TODO: We are assuming that Lovelaces appear first in this list.
              -- {{{
              if sym' == qvfDonationSymbol && amt' == 1 then
                tn'
              else
                traceError "Unauthentic donation UTxO."
              -- }}}
            _                   ->
              -- {{{
              traceError "Donation UTxO has bad multiasset."
              -- }}}
          -- }}}
      in
        traceIfFalse "The project UTxO must also be getting consumed."
      $ xInputWithSpecificDatumExists qvfProjectSymbol tn
      $ \case
          ReceivedDonationsCount _    -> True
          DonationFoldingProgress _ _ -> True
          _                           -> False
      -- }}} 

    (ReceivedDonationsCount soFar                 , FoldDonationsPhaseOne  ) ->
      -- First Phase of Folding Donations
      traceError "TODO."

    (DonationFoldingProgress tot soFar            , FoldDonationsPhaseOne  ) ->
      -- First Phase of Folding Donations
      traceError "TODO."

    (Donations pkhToAmountMap                     , FoldDonationsPhaseTwo  ) ->
      -- Second Phase of Folding Donations
      traceError "TODO."

    (DonationFoldingConcluded tot                 , FoldDonationsPhaseTwo  ) ->
      -- Second Phase of Folding Donations
      traceError "TODO."

    (DonationAccumulationProgress tot ps ds w     , AccumulateDonations    ) ->
      -- Accumulation of Donated Lovelaces
      traceError "TODO."

    (PrizeWeight weight False                     , AccumulateDonations    ) ->
      -- Accumulation of Donated Lovelaces
      traceError "TODO."

    (DonationAccumulationConcluded ps ds den False, PayKeyHolderFee        ) ->
      -- Key Holder Fee Collection
      traceError "TODO."

    (DonationAccumulationConcluded ps ds den True , DistributePrize        ) ->
      -- Prize Distribution
      traceError "TODO."

    (PrizeWeight weight True                      , DistributePrize        ) ->
      -- Prize Distribution
      traceError "TODO."

    (Escrow                                       , WithdrawBounty         ) ->
      -- Bounty Collection from Escrow Account
      traceError "TODO."

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


-- UTILS
-- {{{
{-# INLINABLE registrationInfoToProjectDetials #-}
registrationInfoToProjectDetials :: RegistrationInfo -> ProjectDetails
registrationInfoToProjectDetials RegistrationInfo{..} =
  -- {{{
  ProjectDetails riPubKeyHash riLabel riRequested
  -- }}}
-- }}}


-- CONSTANTS
-- {{{
registrationFee :: Integer
registrationFee = 3_000_000

halfOfTheRegistrationFee :: Integer
halfOfTheRegistrationFee = 1_500_000
-- }}}

-- TxInfo	= TxInfo
--   { txInfoInputs          :: [TxInInfo]
--   , txInfoReferenceInputs :: [TxInInfo]
--   , txInfoOutputs         :: [TxOut]
--   , txInfoFee             :: Value
--   , txInfoMint            :: Value
--   , txInfoDCert           :: [DCert]
--   , txInfoWdrl            :: Map StakingCredential Integer
--   , txInfoValidRange      :: POSIXTimeRange
--   , txInfoSignatories     :: [PubKeyHash]
--   , txInfoRedeemers       :: Map ScriptPurpose Redeemer 
--   , txInfoData            :: Map DatumHash Datum 
--   , txInfoId              :: TxId
--   }

-- TxInInfo = TxInInfo
--   { txInInfoOutRef    :: TxOutRef
--   , txInInfoResolved  :: TxOut
--   }

-- TxOut = TxOut
--   { txOutAddress         :: Address
--   , txOutValue           :: Value
--   , txOutDatum           :: OutputDatum
--   , txOutReferenceScript :: Maybe ScriptHash
--   }

-- OutputDatum
--   = NoOutputDatum	 
--   | OutputDatumHash DatumHash	 
--   | OutputDatum     Datum


