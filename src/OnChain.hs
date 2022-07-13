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
import qualified Plutus.V1.Ledger.Interval   as Interval
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Scripts    (ValidatorHash (..))
import           PlutusTx                    (Data (..))
import qualified PlutusTx
import qualified PlutusTx.AssocMap           as Map
import           PlutusTx.AssocMap           (Map)
import qualified PlutusTx.Builtins           as Builtins
import           PlutusTx.Builtins.Internal  (BuiltinInteger)
import           PlutusTx.Prelude            hiding (unless)
import           PlutusTx.Prelude            (BuiltinByteString, (<>))
import           PlutusTx.Sqrt               (Sqrt (..), isqrt)
import           Schema                      (ToSchema)
-- }}}


-- QVF PARAMETERS
-- {{{
data QVFParams = QVFParams
  { qvfKeyHolder  :: !PubKeyHash
  , qvfSymbol     :: !CurrencySymbol
  , qvfTokenName  :: !TokenName
  , qvfTokenCount :: !Integer
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
-- DONATIONS
-- {{{
data Donations = Donations
  -- Switching to @Map@ for code simplification.
  -- { getDonations :: ![(PubKeyHash, Integer)]
  { getDonations :: !(Map PubKeyHash Integer)
  }

instance Eq Donations where
  {-# INLINABLE (==) #-}
  Donations ds0 == Donations ds1 = ds0 == ds1

instance Semigroup Donations where
  {-# INLINABLE (<>) #-}
  (Donations ds0) <> (Donations ds1) =
    Donations (Map.unionWith (+) ds0 ds1)

instance Monoid Donations where
  {-# INLINABLE mempty #-}
  mempty = Donations Map.empty

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
data QVFInfo = QVFInfo
  { qvfProjects :: ![Project]
  , qvfPool     :: !Integer
  , qvfDeadline :: !(Maybe POSIXTime)
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


{-# INLINABLE pluck #-}
pluck :: (a -> Bool) -> [a] -> Maybe (a, [a])
pluck _ [] = Nothing
pluck p xs =
  -- {{{
  case go xs ([], Nothing, zs) of
    (_, Nothing, _)      ->
      Nothing
    (prevYs, Just y, ys) ->
      Just (y, prevYs ++ ys)
  where
    go [] a = a
    go (y : ys) (soFar, _, zs)
      | p y       =
          (soFar, Just y, ys)
      | otherwise =
          go ys (y : soFar, Nothing, zs)
  -- }}}


{-# INLINABLE mergeProjects #-}
mergeProjects :: [Project] -> [Project] -> [Project]
mergeProjects projects0 projects1 =
  -- {{{
  let
    foldFn :: Project
           -> ([Project], [Project])
           -> ([Project], [Project])
    foldFn project (acc, ps1) =
      -- {{{
      case pluck (== project) ps1 of
        Nothing ->
          (project : acc, ps1)
        Just (p, newPs1)  ->
          let
            newProject =
              project
                {pDonations = pDonations project <> pDonations p}
          in
          (newProject : acc, newPs1)
      -- }}}
    (initAcc, newProjects1)   =
      foldr foldFn ([], projects1) projects0
  in
  initAcc ++ newProjects1
  -- }}}


instance Eq QVFInfo where
  {-# INLINABLE (==) #-}
  qvf0 == qvf1 =
    if qvfPool qvf0 == qvfPool qvf1 then
      -- {{{
      (sortProjects $ qvfProjects qvf0) == (sortProjects $ qvfProjects qvf1)
      -- }}}
    else
      -- {{{
      False
      -- }}}

instance Semigroup QVFInfo where
  {-# INLINABLE (<>) #-}
  (QVFInfo projs0 pool0 mDl0) <> (QVFInfo projs1 pool1 mDl1) =
    QVFInfo
      (mergeProjects projs0 projs1)
      (pool0 + pool1)
      ( case (mDl0, mDl1) of
          (Nothing, Nothing)   -> Nothing
          (Just dl, Nothing)   -> Just dl
          (Nothing, Just dl)   -> Just dl
          (Just dl0, Just dl1) -> Just $ max dl0 dl1
      )

instance Monoid QVFInfo where
  {-# INLINABLE mempty #-}
  mempty = QVFInfo [] 0 Nothing

PlutusTx.unstableMakeIsData ''QVFInfo


data QVFDatum
  = NotStarted
  | InProgress QVFInfo
  | Closed     QVFInfo

instance Eq QVFDatum where
  {-# INLINABLE (==) #-}
  NotStarted       == NotStarted       = True
  InProgress info0 == InProgress info1 = info0 == info1
  Closed     info0 == Closed     info1 = info0 == info1
  _                == _                = False

PlutusTx.makeIsDataIndexed ''QVFDatum
  [ ('NotStarted, 0)
  , ('InProgress, 1)
  , ('Closed    , 2)
  ]
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
  = InitiateFund                  -- ^ Endpoint meant to be invoked by the keyholder for distributing authentication tokens.
  | AddProject AddProjectParams   -- ^ Add projects that can be funded.
  | Donate     DonateParams       -- ^ Attempt donation to a project (identified simply by its public key hash).
  | Contribute Integer            -- ^ Add to the prize pool.
  | Distribute                    -- ^ Distribute the fund to projects per QVF
  | SetDeadline (Maybe POSIXTime) -- ^ Deadline before users can interact with the contract.
 

PlutusTx.makeIsDataIndexed ''QVFAction
  [ ('InitiateFund, 0)
  , ('AddProject  , 1)
  , ('Donate      , 2)
  , ('Contribute  , 3)
  , ('Distribute  , 4)
  , ('SetDeadline , 5)
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
    keyHolder  = qvfKeyHolder qvfParams
    tokenAsset = qvfAsset qvfParams
    info       = scriptContextTxInfo ctx

    -- | Helper function to count the provided
    --   authentication token in a UTxO.
    tokenCountIn :: TxOut -> Integer
    tokenCountIn utxo =
      -- {{{
      assetClassValueOf (txOutValue utxo) tokenAsset
      -- }}}

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

    -- | Checks if the output UTxO is carrying a
    --   specified amount of authenticity tokens.
    outputHasXTokens :: TxOut -> Integer -> Bool
    outputHasXTokens txOut x = tokenCountIn txOut == x

    -- | Possible attached datum to the output UTxO
    --   (may not be properly implemented... could
    --   be valid with a "multi-witness" transaction).
    mOutputDatum :: Maybe QVFDatum
    mOutputDatum =
      -- {{{
      getDatumFromUTxO ownOutput (`findDatum` info)
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

    -- | Checks authenticity of the input/output UTxO's.
    oneTokenInOneTokenOut :: Bool
    oneTokenInOneTokenOut =
      -- {{{
         traceIfFalse "There should be exactly one authentication token in input." (outputHasXTokens ownInput 1)
      && traceIfFalse "There should be exactly one authentication token in output." (outputHasXTokens ownOutput 1)
      -- }}}

    deadlinePassed :: POSIXTime -> Bool
    deadlinePassed dl =
      -- {{{
      let
        txTimeRange = txInfoValidRange info
        dlToInf     = Interval.from dl
      in
      Interval.overlaps txTimeRange dlToInf
      -- }}}

    currDeadlinePassed :: QVFInfo -> Bool
    currDeadlinePassed currDatum =
      -- {{{
      case qvfDeadline currDatum of
        Nothing ->
          False
        Just dl ->
          traceIfTrue 
            "Deadline passed."
            (deadlinePassed dl)
      -- }}}

    signedByKeyHolder :: Bool
    signedByKeyHolder =
      -- {{{
      traceIfFalse
        "Unauthorized."
        (txSignedBy info keyHolder)
      -- }}}
  in
  case (datum, action) of
    (NotStarted          , InitiateFund                ) ->
      -- {{{
      let
        tokenCount      = qvfTokenCount qvfParams
        outputsAreValid =
          -- {{{
          case getContinuingOutputs ctx of
            [] ->
              -- {{{
              traceError "No UTxO is sent back to the address."
              -- }}}
            os ->
              -- {{{
                 all
                   ( \o ->
                       case getDatumFromUTxO o (`findDatum` info) of
                         Nothing ->
                           -- {{{
                           traceError "All outputs should carry datum hashes."
                           -- }}}
                         Just oD ->
                           -- {{{
                              traceIfFalse
                                "All outputs should carry exactly one authentication token."
                                (outputHasXTokens o 1)
                           && traceIfFalse
                                "All outputs should carry exactly the set minimum Lovelace count."
                                (lovelaceFromValue (txOutValue o) == minAuthLovelace)
                           && traceIfFalse
                                "All outputs should carry mempty datums."
                                (oD == InProgress mempty)
                           -- }}}
                   )
                   os
              && length os == tokenCount
              -- }}}
          -- }}}
      in
         traceIfFalse
           "Not all authentication tokens are consumed."
           (outputHasXTokens ownInput tokenCount)
      && traceIfFalse
           "Authentication tokens are not distributed properly."
           outputsAreValid
      && signedByKeyHolder
      -- }}}
    (NotStarted          , _                           ) ->
      -- {{{
      traceError "Funding round not started yet."
      -- }}}
    (_                   , Distribute                  ) ->
      -- {{{
      let
        datumFold inTx acc  =
          -- {{{
          let
            o = txInInfoResolved inTx
          in
          if outputHasXTokens o 1 then
            -- {{{
            case getDatumFromUTxO o (`findDatum` info) of
              Just (InProgress dInfo) ->
                -- {{{
                dInfo <> acc
                -- }}}
              _                       ->
                -- {{{
                traceError "Invalid datum found in input."
                -- }}}
            -- }}}
          else
            -- {{{
            traceError "All inputs must have exactly one authentication token."
            -- }}}
          -- }}}
        currDatum           = foldr datumFold mempty $ txInfoInputs info
        tokenCount          = qvfTokenCount qvfParams
        outputUTxOs         = txInfoOutputs info
        projectCount        = length projects
        validOutputCount    = length outputUTxOs >= projectCount + 2
        allTokensPresent    = outputHasXTokens ownOutput tokenCount
        projects            = qvfProjects currDatum
        -- For each project, the initial registration costs are meant to be
        -- refunded.                              v------------------------v
        initialPool         = qvfPool currDatum - minLovelace * projectCount
        prizeMappings :: [(PubKeyHash, Integer)]
        (extraFromRounding, prizeMappings)    = foldProjects initialPool projects
        foldFn (projPKH, prize) keyHolderFees =
          -- {{{
          let
            forKH = max 0 $ 5 * (prize - minLovelace) `divide` 100
          in
          if (prize - forKH) == lovelaceFromValue (valuePaidTo info projPKH) then
            forKH + keyHolderFees
          else
            traceError "Invalid prize distribution."
          -- }}}
        -- | The folding already checks that each project is getting their
        --   won prize.
        forKeyHolder        =
          -- {{{
          foldr
            foldFn
            (extraFromRounding + (tokenCount - 1) * minAuthLovelace)
            prizeMappings --     ^--------------------------------^
            -- Initial minimum Lovelaces accompanied by each authentication
            -- token should be returned to the key holder. One is reduced for
            -- the @Closed@ datum that is sent back to the script address.
          -- }}}
        keyHolderImbursed   =
          lovelaceFromValue (valuePaidTo info keyHolder) == forKeyHolder
      in
         signedByKeyHolder
      && traceIfFalse
           "Deadline not reached yet."
           (currDeadlinePassed currDatum)
      && traceIfFalse
           "Improper consumption and production of the authentication tokens."
            allTokensPresent
      && traceIfFalse
           "Key holder not imbursed."
           keyHolderImbursed
      && traceIfFalse
           "Invalid output datum hash."
           (isOutputDatumValid $ Closed currDatum)
      && traceIfFalse
           "Invalid output count."
           validOutputCount
      -- }}}
    (InProgress currDatum, AddProject addProjectParams ) ->
      -- {{{
      -- NOTE that there is no prevention mechanism for registering a project
      -- on multiple UTxO's. This should probably be handled off-chain for the
      -- time being.
      case addProjectToDatum addProjectParams currDatum of
        Left err       ->
          traceError err
        Right newDatum ->
             traceIfFalse
               "Project submission fees not paid."
               (enoughAddedInOutput minLovelace)
          && traceIfFalse
               "Invalid output datum hash."
               (isOutputDatumValid $ InProgress newDatum)
          && currDeadlinePassed currDatum
          && oneTokenInOneTokenOut
      -- }}}
    (InProgress currDatum, Donate dPs@DonateParams {..}) ->
      -- {{{
      case addDonationToDatum dPs currDatum of
        Left err       ->
          traceError err
        Right newDatum ->
             traceIfFalse
               "Not enough Lovelaces provided."
               (enoughAddedInOutput dpAmount)
          && traceIfFalse
               "Invalid output datum hash."
               (isOutputDatumValid $ InProgress newDatum)
          && currDeadlinePassed currDatum
          && oneTokenInOneTokenOut
      -- }}}
    (InProgress currDatum, Contribute contribution     ) ->
      -- {{{
      case addContributionToDatum contribution currDatum of
        Left err       ->
          traceError err
        Right newDatum ->
             traceIfFalse
               "Not enough Lovelaces provided."
               (enoughAddedInOutput contribution)
          && traceIfFalse
               "Invalid output datum hash."
               (isOutputDatumValid $ InProgress newDatum)
          && currDeadlinePassed currDatum
          && oneTokenInOneTokenOut
      -- }}}
    (InProgress currDatum, SetDeadline mNewDl          ) ->
      -- {{{
      signedByKeyHolder &&
      case (qvfDeadline currDatum, mNewDl) of
        (_, Just newDl) ->
          -- {{{
             traceIfTrue
               "New deadline is in the past."
               (deadlinePassed newDl)
          && oneTokenInOneTokenOut
          -- Updating only one of the UTxO's suffices (check the @mappend@
          -- implementation of the @QVFInfo@).
          -- }}}
        _               ->
          -- {{{
          True
          -- }}}
      -- }}}
    (Closed _            , _                           ) ->
      -- {{{
      traceError "This funding round has ended."
      -- }}}
    _                                                    ->
      -- {{{
      traceError "Invalid datum/redeemer combination."
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
qvfValidator = Scripts.validatorScript . typedQVFValidator


qvfValidatorHash :: QVFParams -> ValidatorHash
qvfValidatorHash = Scripts.validatorHash . typedQVFValidator


qvfAddress :: QVFParams -> Address
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
    , pDonations  = mempty
    }
  -- }}}


{-# INLINABLE addDonation #-}
addDonation :: PubKeyHash
            -> Integer
            -> Donations
            -> Donations
addDonation donor lovelaces donations =
  -- {{{
  donations <> Donations (Map.singleton donor lovelaces)
  -- case updateIfWith ((== donor) . fst) (fmap (lovelaces +)) kvs of
  --   Nothing     ->
  --     Donations $ (donor, lovelaces) : kvs
  --   Just newKVs ->
  --     Donations newKVs
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
    -- foldFn (_, lovelaces) (pool, votes) =
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


{-# INLINABLE addProjectToDatum #-}
addProjectToDatum :: AddProjectParams
                  -> QVFInfo
                  -> Either BuiltinString QVFInfo
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
    Left "Project already exists."
  -- }}}


{-# INLINABLE addDonationToDatum #-}
addDonationToDatum :: DonateParams
                   -> QVFInfo
                   -> Either BuiltinString QVFInfo
addDonationToDatum DonateParams {..} currDatum =
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
    in
    case mNewProjects of
      Nothing ->
        Left "Target project not found."
      Just newProjects ->
        Right $ currDatum {qvfProjects = newProjects}
  -- }}}


{-# INLINABLE addContributionToDatum #-}
addContributionToDatum :: Integer
                       -> QVFInfo
                       -> Either BuiltinString QVFInfo
addContributionToDatum contribution currDatum =
  -- {{{
  if contribution < minLovelace then
    Left "Donation less than minimum."
  else
    Right $ currDatum {qvfPool = contribution + qvfPool currDatum}
  -- }}}


{-# INLINABLE updateDatum #-}
updateDatum :: QVFAction -> QVFDatum -> Either BuiltinString QVFDatum
updateDatum action datum =
  -- {{{
  case (datum, action) of
    (InProgress currDatum, AddProject addProjectParams) ->
      -- {{{
      InProgress <$> (addProjectToDatum addProjectParams currDatum)
      -- }}}
    (InProgress currDatum, Donate donateParams        ) ->
      -- {{{
      InProgress <$> (addDonationToDatum donateParams currDatum)
      -- }}}
    (InProgress currDatum, Contribute contribution    ) ->
      -- {{{
      InProgress <$> (addContributionToDatum contribution currDatum)
      -- }}}
    (InProgress currDatum, SetDeadline mNewDeadline   ) ->
      -- {{{
      -- Since this function is only used by the CLI application, this
      -- invokation results to a non-conditional acceptance of the new deadline.
      Right $ InProgress $ currDatum {qvfDeadline = mNewDeadline}
      -- }}}
    _                                                   ->
      -- {{{
      Left "No datum needed."
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
minAuthLovelace = 10_000_000
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


