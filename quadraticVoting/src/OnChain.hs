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
module OnChain where
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
import           Prelude                     (Show, show, String)
import qualified Prelude                     as P
import           Utils
-- }}}


-- QVF PARAMETERS
-- {{{
data QVFParams = QVFParams
  { qvfKeyHolder  :: !PubKeyHash
  , qvfSymbol     :: !CurrencySymbol
  , qvfTokenName  :: !TokenName
  }

PlutusTx.makeLift ''QVFParams

{-# INLINABLE qvfAsset #-}
qvfAsset :: QVFParams -> AssetClass
qvfAsset qvf =
  AssetClass
    ( qvfSymbol    qvf
    , qvfTokenName qvf
    )
-- }}}


-- DATUM
-- {{{
-- PROJECT
-- {{{
data Project = Project
  { pPubKeyHash :: !PubKeyHash
  , pLabel      :: !BuiltinByteString
  , pRequested  :: !Integer
  , pDonations  :: !(Map PubKeyHash Integer)
  }

showDonations :: Map PubKeyHash Integer -> String
showDonations ds =
  -- {{{
  let
    vs      = Map.elems ds
    total   = lovelaceToAda $ foldr (+) 0 vs
    dsCount = length ds
  in
       "Total of "
  P.++ show total
  P.++ " Ada, by "
  P.++ show dsCount
  P.++ " donor(s)."
  -- }}}

instance Show Project where
  show Project {..} =
    -- {{{
         "\t{ Label:     " P.++ show          pLabel     P.++ "\n"
    P.++ "\t, Donations: " P.++ showDonations pDonations P.++ "\n"
    P.++ "\t}\n"
    -- }}}

instance Eq Project where
  {-# INLINABLE (==) #-}
  a == b = pPubKeyHash a == pPubKeyHash b
  -- It suffices to compare the public key hashes.

PlutusTx.unstableMakeIsData ''Project
-- }}}


-- QVF DATUM
-- {{{
data QVFDatum = QVFDatum
  { qvfProjects   :: ![Project]
  , qvfPool       :: !Integer
  , qvfDeadline   :: !POSIXTime
  , qvfInProgress :: !Bool
  }

instance Show QVFDatum where
  show QVFDatum {..} =
    -- {{{
    ( if qvfInProgress then 
        "\n<<<  IN PROGRESS  >>>\n"
      else
        "\n<<< FUNDING ENDED >>>\n"
    ) P.++ "Projects:\n"   P.++ P.concatMap show qvfProjects
      P.++ "Pool:\n\t"     P.++ show (lovelaceToAda qvfPool) P.++ " Ada\n"
      P.++ "Deadline:\n\t" P.++ show qvfDeadline P.++ "\n"
    -- }}}

instance Eq QVFDatum where
  {-# INLINABLE (==) #-}
  qvf0 == qvf1 =
    -- {{{
    let
      cond =
           qvfPool       qvf0 == qvfPool       qvf1
        && qvfDeadline   qvf0 == qvfDeadline   qvf1
        && qvfInProgress qvf0 == qvfInProgress qvf1
    in
    if cond then
      -- {{{
      let
        go []         []  = True
        go []         _   = False
        go _          []  = False
        go (p0 : ps0) ps1 =
          case pluck (== p0) ps1 of
            Nothing          -> False
            Just (_, newPs1) -> go ps0 newPs1
      in
      go (qvfProjects qvf0) (qvfProjects qvf1)
      -- }}}
    else
      -- {{{
      False
      -- }}}
    -- }}}

initialDatum :: POSIXTime -> QVFDatum
initialDatum dl = QVFDatum
  { qvfProjects   = []
  , qvfPool       = 0
  , qvfDeadline   = dl
  , qvfInProgress = True
  }

PlutusTx.unstableMakeIsData ''QVFDatum
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
  = AddProject AddProjectParams  -- ^ Add projects that can be funded.
  | Donate     [DonateParams]    -- ^ Attempt donation to a list of projects (identified simply by their public key hashes).
  | Contribute Integer           -- ^ Add to the prize pool.
  | Distribute                   -- ^ Distribute the fund to projects per QVF
  | SetDeadline POSIXTime        -- ^ Deadline before users can interact with the contract.
 

PlutusTx.makeIsDataIndexed ''QVFAction
  [ ('AddProject  , 0)
  , ('Donate      , 1)
  , ('Contribute  , 2)
  , ('Distribute  , 3)
  , ('SetDeadline , 4)
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
mkQVFValidator qvfParams currDatum@QVFDatum {..} action ctx =
  -- {{{
  let
    keyHolder  = qvfKeyHolder qvfParams
    tokenAsset = qvfAsset qvfParams
    info       = scriptContextTxInfo ctx

    -- | UTxO sitting at this script address being spent.
    ownInput :: TxOut
    ownInput =
      -- {{{
      case findOwnInput ctx of
        Nothing ->
          traceError "E1"
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
          traceError "E2"
      -- }}}

    -- | Checks if UTxO is carrying an authenticity tokens.
    outputHasOneToken :: TxOut -> Bool
    outputHasOneToken utxo =
      -- {{{
      assetClassValueOf (txOutValue utxo) tokenAsset == 1
      -- }}}

    -- | Possible attached datum to the output UTxO.
    mOutputDatum :: Maybe QVFDatum
    mOutputDatum =
      -- {{{
      getDatumFromUTxO ownOutput (`findDatum` info)
      -- }}}

    -- | Checks if the attached output datum is properly updated.
    isOutputDatumValid :: QVFDatum -> Bool
    isOutputDatumValid newDatum =
      -- {{{
      case mOutputDatum of
        Nothing ->
          False
        Just outputDatum ->
          outputDatum == newDatum
      -- }}}

    -- | Helper function to see if the given UTxO has the given amount of
    --   Lovelaces.
    utxoHasLovelaces :: TxOut -> Integer -> Bool
    utxoHasLovelaces txOut lovelaces =
      -- {{{
      lovelaceFromValue (txOutValue txOut) == lovelaces
      -- }}}

    -- | Helper function to see if Lovelace count of @ownOutput@ has
    --   increased by a given amount.
    enoughAddedInOutput :: Integer -> Bool
    enoughAddedInOutput addedAmount =
      -- {{{
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
      outVal == (inVal <> Ada.lovelaceValueOf addedAmount)
      -- }}}

    -- | Checks authenticity of the input/output UTxO's.
    oneTokenInOneTokenOut :: Bool
    oneTokenInOneTokenOut =
      -- {{{
         traceIfFalse "E3" (outputHasOneToken ownInput)
      && traceIfFalse "E4" (outputHasOneToken ownOutput)
      -- }}}

    -- | Checks whether the deadline hasn't been reached yet.
    canInteract :: Bool
    canInteract =
      Interval.to qvfDeadline `Interval.contains` txInfoValidRange info

    -- | Checks whether the deadline has been reached.
    canDistribute :: Bool
    canDistribute =
      Interval.from qvfDeadline `Interval.contains` txInfoValidRange info

    -- | Turned into a function to impose laziness.
    signedByKeyHolder :: () -> Bool
    signedByKeyHolder _ =
      -- {{{
      traceIfFalse "E6" (txSignedBy info keyHolder)
      -- }}}
  in
  if qvfInProgress then
    case action of
      AddProject addProjectParams  ->
        -- {{{
        case addProjectToDatum addProjectParams currDatum of
          Left err       ->
            traceError err
          Right newDatum ->
               traceIfFalse "E23" (enoughAddedInOutput minLovelace)
            && traceIfFalse "E24" (isOutputDatumValid newDatum)
            && traceIfFalse "E5"  canInteract
            && oneTokenInOneTokenOut
        -- }}}
      Donate dPs                   ->
        -- {{{
        case addDonationsToDatum dPs currDatum of
          Left err       ->
            traceError err
          Right (totalDonations, newDatum) ->
               traceIfFalse "E25" (enoughAddedInOutput totalDonations)
            && traceIfFalse "E26" (isOutputDatumValid newDatum)
            && traceIfFalse "E5"  canInteract
            && oneTokenInOneTokenOut
        -- }}}
      Contribute contribution      ->
        -- {{{
        case addContributionToDatum contribution currDatum of
          Left err       ->
            traceError err
          Right newDatum ->
               traceIfFalse "E27" (enoughAddedInOutput contribution)
            && traceIfFalse "E28" (isOutputDatumValid newDatum)
            && traceIfFalse "E5"  canInteract
            && oneTokenInOneTokenOut
        -- }}}
      SetDeadline newDl            ->
        -- {{{
        case setDeadlineOfDatum newDl currDatum of
          Left err       ->
            traceError err
          Right newDatum ->
               traceIfFalse "E28" (isOutputDatumValid newDatum)
            && oneTokenInOneTokenOut
            && signedByKeyHolder ()
        -- }}}
      Distribute                   ->
        -- {{{
        let
          newDatum            = currDatum {qvfInProgress = False}
          projectCount        = length qvfProjects
          -- For each project, the initial registration costs are meant to be
          -- refunded.                    v------------------------v
          initialPool         = qvfPool - minLovelace * projectCount
          prizeMappings :: [(PubKeyHash, Integer)]
          (extraFromRounding, prizeMappings)    =
            foldProjects initialPool qvfProjects
          foldFn (projPKH, prize) keyHolderFees =
            -- {{{
            let
              (finalPrize, forKH) = getFinalPrizeAndKeyHoldersFee prize
            in
            if finalPrize == lovelaceFromValue (valuePaidTo info projPKH) then
              forKH + keyHolderFees
            else
              traceError "E17"
            -- }}}
          -- | The folding already checks that each project is getting their
          --   won prize.
          forKeyHolder        = foldr foldFn extraFromRounding prizeMappings
          keyHolderImbursed   =
            lovelaceFromValue (valuePaidTo info keyHolder) >= forKeyHolder
        in
           traceIfFalse "E18" canDistribute
        && traceIfFalse "E38" (utxoHasLovelaces ownOutput minAuthLovelace)
        && traceIfFalse "E20" keyHolderImbursed
        && traceIfFalse "E21" (isOutputDatumValid newDatum)
        && oneTokenInOneTokenOut
        && signedByKeyHolder ()
        -- }}}
  else
    -- {{{
    traceError "E30"
    -- }}}
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
{-# INLINABLE initiateProject #-}
initiateProject :: AddProjectParams -> Project
initiateProject AddProjectParams {..} =
  -- {{{
  Project
    { pPubKeyHash = appPubKeyHash
    , pLabel      = appLabel
    , pRequested  = appRequested
    , pDonations  = Map.empty
    }
  -- }}}


{-# INLINABLE addDonation #-}
addDonation :: PubKeyHash
            -> Integer
            -> Map PubKeyHash Integer
            -> Map PubKeyHash Integer
addDonation donor lovelaces donations =
  -- {{{
  Map.unionWith (+) donations (Map.singleton donor lovelaces)
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
foldDonations :: Map PubKeyHash Integer -> (Integer, Integer)
foldDonations ds =
  -- {{{
  let
    foldFn lovelaces (pool, votes) =
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
    traceError "E32"
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
        , (p, prize + minLovelace) : ps2prizes
        ) --          ^---------^
        ---- Returning the initial cost of project registration.
        -- }}}
      (distributedPrizes, projectsToPrizes) =
        foldr finalFoldFn (0, []) addrsToVotes
    in
    ( prizePool - distributedPrizes
    , projectsToPrizes
    )
    -- }}}
  -- }}}


{-# INLINABLE getFinalPrizeAndKeyHoldersFee #-}
getFinalPrizeAndKeyHoldersFee :: Integer -> (Integer, Integer)
getFinalPrizeAndKeyHoldersFee initPrize =
  -- {{{
  let
    forKH = max 0 $ 5 * (initPrize - minLovelace) `divide` 100
  in
  (initPrize - forKH, forKH)
  -- }}}


{-# INLINABLE addProjectToDatum #-}
addProjectToDatum :: AddProjectParams
                  -> QVFDatum
                  -> Either BuiltinString QVFDatum
addProjectToDatum addProjectParams currDatum =
  -- {{{
  let
    newProject   = initiateProject addProjectParams
    currProjects = qvfProjects currDatum
    projectIsNew = notElem newProject currProjects
  in
  if projectIsNew then
    Right $
      currDatum
        { qvfProjects = newProject : currProjects
        , qvfPool     = qvfPool currDatum + minLovelace
        }
  else
    Left "E33"
  -- }}}


{-# INLINABLE addDonationToDatum #-}
addDonationToDatum :: DonateParams
                   -> QVFDatum
                   -> Either BuiltinString QVFDatum
addDonationToDatum DonateParams {..} currDatum =
  -- {{{
  if dpAmount < minLovelace then
    Left "E34"
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
    in
    case mNewProjects of
      Nothing ->
        Left "E35"
      Just newProjects ->
        Right $ currDatum {qvfProjects = newProjects}
  -- }}}


{-# INLINABLE addDonationsToDatum #-}
addDonationsToDatum :: [DonateParams]
                    -> QVFDatum
                    -> Either BuiltinString (Integer, QVFDatum)
addDonationsToDatum dPs currDatum =
  -- {{{
  let
    foldFn dP (acc, info) =
      (acc + dpAmount dP,) <$> addDonationToDatum dP info
  in
  foldrM foldFn (0, currDatum) dPs
  -- }}}


{-# INLINABLE addContributionToDatum #-}
addContributionToDatum :: Integer
                       -> QVFDatum
                       -> Either BuiltinString QVFDatum
addContributionToDatum contribution currDatum =
  -- {{{
  if contribution < minLovelace then
    Left "E36"
  else
    Right $ currDatum {qvfPool = contribution + qvfPool currDatum}
  -- }}}


{-# INLINABLE setDeadlineOfDatum #-}
setDeadlineOfDatum :: POSIXTime
                   -> QVFDatum
                   -> Either BuiltinString QVFDatum
setDeadlineOfDatum newDeadline currDatum =
  Right $ currDatum {qvfDeadline = newDeadline}


{-# INLINABLE updateDatum #-}
updateDatum :: QVFAction -> QVFDatum -> Either BuiltinString QVFDatum
updateDatum action currDatum@QVFDatum {..} =
  -- {{{
  case (qvfInProgress, action) of
    (False, _                          ) ->
      -- {{{
      Left "E30"
      -- }}}
    (True,  AddProject addProjectParams) ->
      -- {{{
      addProjectToDatum addProjectParams currDatum
      -- }}}
    (True,  Donate donateParams        ) ->
      -- {{{
      snd <$> addDonationsToDatum donateParams currDatum
      -- }}}
    (True,  Contribute contribution    ) ->
      -- {{{
      addContributionToDatum contribution currDatum
      -- }}}
    (True,  SetDeadline newDeadline    ) ->
      -- {{{
      -- Since this function is only used by the CLI application, this
      -- is an unconditional acceptance of the new deadline.
      setDeadlineOfDatum newDeadline currDatum
      -- }}}
    (True,  Distribute                 ) ->
      -- {{{
      -- Simply closes the fund.
      Right $ currDatum {qvfInProgress = False}
      -- }}}
  -- }}}
-- }}}


-- CONSTANTS
-- {{{
minLovelace :: Integer
minLovelace = 2_000_000

-- | Bigger value than the required minimum to
--   leave some "headspace."
minAuthLovelace :: Integer
minAuthLovelace = 2_000_000
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

-- TxOutRef = TxOutRef
--   { txOutRefId  :: TxId
--   , txOutRefIdx :: Integer
--   }
--
-- TxId = TxId
--   { getTxId :: BuiltinByteString
--   }

-- TxOut = TxOut
--   { txOutAddress      :: Address
--   , txOutValue        :: Value
--   , txOutDatumHash    :: Maybe DatumHash
--   }


