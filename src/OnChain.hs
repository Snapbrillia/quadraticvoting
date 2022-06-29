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
import qualified Ledger.Ada                  as Ada
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


-- QVF PARAMETERS
-- {{{
data QVFParams = QVFParams
  { qvfKeyHolder :: !PubKeyHash
  , qvfSymbol    :: !CurrencySymbol
  }

PlutusTx.makeLift ''QVFParams
-- }}}


-- DATUM
-- {{{
-- DONATIONS
-- {{{
data Donations = Donations
  { getDonations :: ![(PubKeyHash, Integer)]
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
  { pPubKeyHash :: !PubKeyHash
  , pLabel      :: !BuiltinByteString
  , pRequested  :: !Integer
  , pDonations  :: !Donations
  }

instance Eq Project where
  {-# INLINABLE (==) #-}
  a == b = (pPubKeyHash a == pPubKeyHash b)
  -- It suffices to compare the public key hashes.

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
          (getPubKeyHash $ pPubKeyHash p0)
          (getPubKeyHash $ pPubKeyHash p1)
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

initialDatum :: QVFDatum
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
  { appPubKeyHash :: !PubKeyHash
  , appLabel      :: !BuiltinByteString
  , appRequested  :: !Integer
  }

PlutusTx.unstableMakeIsData ''AddProjectParams
-- }}}


-- DONATE PARAMETERS
-- {{{
data DonateParams = DonateParams
  { dpDonor   :: !PubKeyHash
  , dpProject :: !PubKeyHash
  , dpAmount  :: !Integer
  }

PlutusTx.unstableMakeIsData ''DonateParams
-- }}}


-- QVF ACTION
-- {{{
data QVFAction
  = AddProject AddProjectParams -- ^ Add projects that can be funded.
  | Donate     DonateParams     -- ^ Attempt donation to a project (identified simply by its public key hash).
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
    --   be valid with a "multi-witness" transaction).
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
    AddProject _             ->
      -- {{{
      case updateDatum action currDatum of
        Left err       ->
          traceError err
        Right newDatum ->
             traceIfFalse
               "Project submission fees not paid."
               (enoughAddedInOutput minLovelace)
          && traceIfFalse
               "Invalid output datum hash."
               (isOutputDatumValid newDatum)
      -- }}}
    Donate DonateParams {..} ->
      -- {{{
      case updateDatum action currDatum of
        Left err       ->
          traceError err
        Right newDatum ->
             traceIfFalse
               "Not enough Lovelaces provided."
               (enoughAddedInOutput dpAmount)
          && traceIfFalse
               "Invalid output datum hash."
               (isOutputDatumValid newDatum)
      -- }}}
    Contribute contribution  ->
      -- {{{
      case updateDatum action currDatum of
        Left err       ->
          traceError err
        Right newDatum ->
             traceIfFalse
               "Not enough Lovelaces provided."
               (enoughAddedInOutput contribution)
          && traceIfFalse
               "Invalid output datum hash."
               (isOutputDatumValid newDatum)
      -- }}}
    Distribute               ->
      -- {{{
      let
        prizeMappings :: [(PubKeyHash, Integer)]
        (extraFromRounding, prizeMappings) =
          foldProjects (qvfPool currDatum) (qvfProjects currDatum)
        mapFn (projPKH, projPrize) =
          -- {{{
          if projPrize == lovelaceFromValue (valuePaidTo info projPKH) then
            True
          else
            -- let
            --   projLabel =
            --     fmap pLabel
            --       (find ((== projPKH) . pPubKeyHash) qvfProjects)
            -- in
            traceError "Invalid prize distribution."
          -- }}}
        -- | Checks whether the distribution follows
        --   the quadratic formula.
        correctlyDistributed = all mapFn prizeMappings
        keyHolderImbursed =
           (lovelaceFromValue (valuePaidTo info keyHolder))
             >= (minLovelace + extraFromRounding)
      in
         traceIfFalse
           "Unauthorized."
           (txSignedBy info keyHolder)
      && traceIfFalse
           "Improper distribution."
           correctlyDistributed
      && traceIfFalse
           "Key holder not imbursed."
           keyHolderImbursed
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
-- | Calls `traceError` if a negative input is given.
takeSqrt :: Integer -> Integer
takeSqrt val =
  -- {{{
  case isqrt val of
    Imaginary ->
      traceError "Square root of a negative number."
    Exactly sqrt ->
      sqrt
    Approximately sqrt ->
      sqrt
  -- }}}


{-# INLINABLE lovelaceFromValue #-}
lovelaceFromValue :: Value -> Integer
lovelaceFromValue = Ada.getLovelace . Ada.fromValue


{-# INLINABLE updateIfWith #-}
-- | If an element of the given list satisfies the
--   predicate, that single element is updated,
--   and the new list is returned (applies to the
--   leftmost item only). If no items satify the
--   predicate, @Nothing@ is returned.
updateIfWith :: (a -> Bool) -> (a -> a) -> [a] -> Maybe [a]
updateIfWith predicate fn xs =
  -- {{{
  let
    -- go :: ([a], Maybe a, [a]) -> ([a], Maybe a, [a])
    go acc@([], Nothing, _) = acc
    go acc@(_ , Just _ , _) = acc
    go (p : ps, Nothing, acc)
      | predicate p = (ps, Just $ fn p, acc)
      | otherwise   = go (ps, Nothing, p : acc)
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
    { pPubKeyHash = appPubKeyHash
    , pLabel      = appLabel
    , pRequested  = appRequested
    , pDonations  = Donations []
    }
  -- }}}


{-# INLINABLE addDonation #-}
addDonation :: PubKeyHash -> Integer -> Donations -> Donations
addDonation donor lovelaces (Donations kvs) =
  -- {{{
  case updateIfWith ((== donor) . fst) (fmap (lovelaces +)) kvs of
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


{-# INLINABLE foldDonations #-}
-- | Notating Lovelace contributions to each project as 
--   \(v\), this is the quadratic formula to represent
--   vote count:
--   \[
--       V = (\sum{\sqrt{v}})^2
--   \]
foldDonations :: Donations -> (Integer, Integer)
foldDonations (Donations ds) =
  -- {{{
  let
    foldFn (_, lovelaces) (pool, votes) =
      ( pool + lovelaces
      , takeSqrt lovelaces + votes
      )
    (total, initVotes) = foldr foldFn (0, 0) ds
  in
  (total, initVotes * initVotes)
  -- }}}


{-# INLINABLE foldProjects #-}
-- | To find how much money each project receives, we first find
--   share of pool (\(c\)) with:
--   \[
--       c = \frac{v}{\sum{V}}
--   \]
--   And finally find the matched amount (\(k\)) by multiplying \(c\)
--   with the total contributions, \(P\):
--   \[
--       k = c * P
--   \]
--   As \(\sum{k}\) can be smaller than \(P\), this function also returns
--   the difference between the two, \(r\):
--   \[
--       r = P - \sum{k}
--   \]
foldProjects :: Integer -> [Project] -> (Integer, [(PubKeyHash, Integer)])
foldProjects initialPool ps =
  -- {{{
  let
    foldFn :: Project
           -> (Integer, [(PubKeyHash, Integer)], Integer)
           -> (Integer, [(PubKeyHash, Integer)], Integer)
    foldFn p (pool, votes, accVotes) =
      -- {{{
      let
        (projsFunds, projsVotes) = foldDonations $ pDonations p
      in
      ( pool + projsFunds
      , (pPubKeyHash p, projsVotes) : votes
      , accVotes + projsVotes
      )
      -- }}}
    (prizePool, addrsToVotes, totalVotes) =
      -- {{{
      foldr foldFn (initialPool, [], 0) ps
      -- }}}
  in
  if totalVotes <= 0 then
    -- {{{
    traceError "No votes cast."
    -- }}}
  else
    -- {{{
    let
      finalFoldFn :: (PubKeyHash, Integer)
                  -> (Integer, [(PubKeyHash, Integer)])
                  -> (Integer, [(PubKeyHash, Integer)])
      finalFoldFn (p, votes) (accPrizes, ps2prizes) =
        -- {{{
        let
          -- @divide@ rounds down.
          prize = (votes * prizePool) `divide` totalVotes
        in
        ( accPrizes + prize
        , (p, prize) : ps2prizes
        )
        -- }}}
      (distributedPrizes, projectsToPrizes) =
        foldr finalFoldFn (0, []) addrsToVotes
    in
    ( prizePool - distributedPrizes
    , projectsToPrizes
    )
    -- }}}
  -- }}}


{-# INLINABLE updateDatum #-}
updateDatum :: QVFAction -> QVFDatum -> Either BuiltinString QVFDatum
updateDatum action currDatum =
  case action of
    AddProject addProjectParams ->
      -- {{{
      let
        newProject   = initiateProject addProjectParams
        currProjects = qvfProjects currDatum
        projectIsNew = notElem newProject currProjects
        newDatum     = currDatum {qvfProjects = newProject : currProjects}
      in
      if projectIsNew then
        Right newDatum
      else
        Left "Project already exists."
      -- }}}
    Donate DonateParams {..}    ->
      -- {{{
      if dpAmount < minLovelace then
        Left "Donation less than minimum."
      else
        let
          currProjects = qvfProjects currDatum
          mNewProjects =
            -- {{{
            updateIfWith
              ((== dpProject) . pPubKeyHash)
              (addDonationToProject dpDonor dpAmount)
              currProjects
            -- }}}
          mNewDatum    =
            (\newPs -> currDatum {qvfProjects = newPs}) <$> mNewProjects
        in
        case mNewDatum of
          Nothing ->
            Left "Target project not found."
          Just newDatum ->
            Right newDatum
      -- }}}
    Contribute contribution     ->
      -- {{{
      if contribution < minLovelace then
        Left "Donation less than minimum."
      else
        Right $ currDatum {qvfPool = contribution + qvfPool currDatum}
      -- }}}
    Distribute                  ->
      -- {{{
      Left "No datum needed."
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


