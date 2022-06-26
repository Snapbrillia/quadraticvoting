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
  { getDonations :: [(!PubKeyHash, !Integer)]
  }

{-# INLINABLE sortDonations #-}
sortDonations :: [(PubKeyHash, Integer)] -> [(PubKeyHash, Integer)]
sortDonations =
  -- {{{
  sortBy
    ( \(PubKeyHash bs0, _) (PubKeyHash bs1, _) ->
        compare bs0 bs1
    )
  -- }}}

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
data QVFDatum = QVFDatum
  { qvfProjects :: ![Project]
  , qvfPool     :: !Integer
  }


{-# INLINABLE sortProjects #-}
sortProjects :: [Project] -> [Project]
sortProjects =
  -- {{{
  sortBy
    ( \p0 p1 ->
        compare
          (addressToBuiltinByteString $ pAddress p0)
          (addressToBuiltinByteString $ pAddress p1)
    )
  -- }}}

instance Eq QVFDatum where
  {-# INLINABLE (==) #-}
  qvf0 == qvf1 =
    if qvfPool qvf0 == qvfPool qvf1 then
      -- {{{
      let
        projs0                   = qvfProjects qvf0
        projs1                   = qvfProjects qvf1
        go res []       []       = res
        go _   []       _        = False
        go _   _        []       = False
        -- Plutus is not lazy and (&&) would be inefficient.
        go res (x : xs) (y : ys) =
          if res then
            go (x == y) xs ys
          else
            False
      in
      go True (sortProjects projs0) (sortProjects projs1)
      -- }}}
    else
      -- {{{
      False
      -- }}}

PlutusTx.unstableMakeIsData ''QVFDatum

initialDatum = QVFDatum [] 0
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
  { dpDonor   :: !PubKeyHash
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
  | Contribute Integer          -- ^ Add to the prize pool.
  | Distribute                  -- ^ Distribute the fund to projects per QVF
 

PlutusTx.makeIsDataIndexed ''QVFAction
  [ ('AddProject, 0)
  , ('Donate    , 1)
  , ('Contribute, 2)
  , ('Distribute, 3)
  ]
-- }}}
-- }}}


-- QVF VALIDATOR 
-- {{{
{-# INLINABLE mkQVFValidator #-}
mkQVFValidator :: PubKeyHash
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
    --   (may not be properly implemented... could
    --   be valid with a "multi-witness transaction).
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
    enoughAddedInOutput :: Integer -> Bool
    enoughAddedInOutput addedAmount =
      -- {{{
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
      outVal == (inVal <> Ada.lovelaceValueOf addedAmount)
      -- }}}
  in
  traceIfFalse "Invalid input datum hash." validInputDatum &&
  case action of
    AddProject addProjectParams ->
      -- {{{
      let
        newProject   = initiateProject addProjectParams
        currProjects = qvfProjects currDatum
        projectIsNew = notElem newProject currProjects
        newDatum     = currDatum {qvfProjects = newProject : currProjects}
      in
         traceIfFalse
           "Project submission fees not paid."
           (enoughAddedInOutput minLovelace)
      && traceIfFalse
           "Project already exists."
           projectIsNew
      && traceIfFalse
           "Invalid output datum hash."
           (isOutputDatumValid newDatum)
      -- }}}
    Donate DonateParams {..}    ->
      -- {{{
      let
        currProjects = qvfProjects currDatum
        mNewProjects =
          -- {{{
          updateIf
            (\p -> pAddress p == dpProject)
            (addDonationToProject dpDonor dpAmount)
            currProjects
          -- }}}
        newDatum     =
          -- {{{
          case mNewProjects of
            Nothing ->
              traceError "Target project not found."
            Just newPs ->
              currDatum {qvfProjects = newPs}
          -- }}}
      in
         traceIfFalse
           "Donation less than minimum."
           (dpAmount < minLovelace)
      && traceIfFalse
           "Not enough Lovelaces provided."
           (enoughAddedInOutput dpAmount)
      && traceIfFalse
           "Invalid output datum hash."
           (isOutputDatumValid newDatum)
      -- }}}
    Contribute contribution     ->
      -- {{{
      let
        newDatum = currDatum {qvfPool = contribution + qvfPool currDatum}
      in
         traceIfFalse
           "Donation less than minimum."
           (contribution < minLovelace)
      && traceIfFalse
           "Not enough Lovelaces provided."
           (enoughAddedInOutput contribution)
      && traceIfFalse
           "Invalid output datum hash."
           (isOutputDatumValid newDatum)
      -- }}}
    Distribute                  ->
      -- {{{
      let
        allOutputs = txInfoOutputs info
        -- | Should check whether the distribution follows
        --   the quadratic formula (not implemented yet).
        correctlyDistributed = True -- TODO
      in
         traceIfFalse
           "Unauthorized."
           (txSignedBy info keyHolder)
      && traceIfFalse
           "Improper distribution."
           correctlyDistributed
      -- }}}
  -- }}}


-- TEMPLATE HASKELL, BOILERPLATE, ETC. 
-- {{{
data QVF
instance Scripts.ValidatorTypes QVF where
    type DatumType    QVF = QVFDatum
    type RedeemerType QVF = QVFAction


typedQVFValidator :: PubKeyHash -> Scripts.TypedValidator QVF
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


qvfValidator :: PubKeyHash -> Validator
qvfValidator = Scripts.validatorScript . typedQVFValidator


qvfValidatorHash :: PubKeyHash -> ValidatorHash
qvfValidatorHash = Scripts.validatorHash . typedQVFValidator


qvfAddress :: PubKeyHash -> Address
qvfAddress = scriptAddress . qvfValidator
-- }}}
-- }}}


-- UTILS 
-- {{{
{-# INLINABLE takeSqrt #-}
takeSqrt :: Integer -> Maybe Integer
takeSqrt val =
  -- {{{
  case isqrt val of
    Imaginary ->
      Nothing
    Exactly sqrt ->
      Just sqrt
    Approximately sqrt ->
      Just sqrt
  -- }}}


{-# INLINABLE updateIf #-}
-- | If an element of the given list satisfies the
--   predicate, that single element is updated,
--   and the new list is returned (applies to the
--   leftmost item only). If no items satify the
--   predicate, @Nothing@ is returned.
updateIf :: (a -> Bool) -> (a -> a) -> [a] -> Maybe [a]
updateIf pred fn xs =
  -- {{{
  let
    go :: ([a], Maybe a, [a]) -> ([a], Maybe a, [a])
    go acc@([], Nothing, _) = acc
    go acc@(_ , Just _ , _) = acc
    go (p : ps, Nothing, acc)
      | pred p    = (ps, Just $ fn p, acc)
      | otherwise = go (ps, Nothing, p : acc)
  in
  case go (xs, Nothing, []) of
    (_, Nothing, _) ->
      Nothing
    (leftXs, Just updatedX, rightXs) ->
      Just $ leftXs ++ (updatedX : rightXs)
  -- }}}


{-# INLINABLE initiateProject #-}
initiateProject :: AddProjectParams -> Project
initiateProject AddProjectParams {..} =
  -- {{{
  Project
    { pAddress   = appAddress
    , pLabel     = appLabel
    , pRequested = appRequested
    , pDonations = Donations []
    }
  -- }}}


{-# INLINABLE addDonation #-}
addDonation :: PubKeyHash -> Integer -> Donations -> Donations
addDonation donor lovelaces (Donations kvs) =
  -- {{{
  case updateIf (\(d, _) -> d == donor) (\(k, v) -> (k, v + lovelaces)) kvs of
    Nothing     ->
      Donations $ (donor, lovelaces) : kvs
    Just newKVs ->
      Donations newKVs
  -- }}}


{-# INLINABLE addDonationToProject #-}
addDonationToProject :: PubKeyHash -> Integer -> Project -> Project
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
  dh      <- txOutDatumHash
  Datum d <- converter dh
  PlutusTx.fromBuiltinData d
  -- }}}


{-# INLINABLE applyQVF #-}
applyQVF :: [Project] -> ()
applyQVF ps =
  let
    foldFn (pool, votes) p = undefined
  in
  undefined
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


