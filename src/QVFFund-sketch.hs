{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Week01.QVFRound
    , LaunchParams (..), DonationParams (..), CloseParams (..)
    , QVFFundSchema
    , launch, open, donate, close, distribute
    , endpoints
    , schemas
    , correctOpenSlotRange
    , printJson
    , printSchemas
    , registeredKnownCurrencies
    , stage
    ) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada
import           Playground.Contract  (IO, ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (unless)
import qualified Prelude              as P
import           Schema               (ToSchema)
import           Text.Printf          (printf)

minLovelace :: Integer
minLovelace = 2000000

-- Description of a QVF fund
data QVFFund = QVFFund
    {
      -- the name of the fund
      fName        :: !BuiltinByteString
      -- the address from which the fund was launched
      fKeyHolder   :: !PaymentPubKeyHash
      -- the time at which the fund opens for donations
    , fOpeningTime :: !POSIXTime
      -- the time after which refund requests are denied
      -- denying refunds during a period leading up to fClosingTime
      -- this could prevent a form of gaming - if refunds are allowed
      -- , fRefundDeadline:: !POSIXTime
      -- the time at which the fund closes for donations
    , fClosingTime :: !POSIXTime
      -- The minimum donation allowed
    , fMinDonation :: !Integer

    -- the grease needed to make this work
    , fCurrency    :: !CurrencySymbol
    , fToken       :: !TokenName
    } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq QVFFund where
    {-# INLINABLE (==) #-}
    a == b = (fName a == fName b) &&
             (fKeyHolder a == fKeyHolder b) &&
             (fOpeningTime a == fOpeningTime b) &&
             (fClosingTime a == fClosingTime b) &&
             (fMinDonation a == fMinDonation b) &&
             (fCurrency a == fCurrency b) &&
             (fToken a == fToken b)

PlutusTx.unstableMakeIsData ''QVFFund
PlutusTx.makeLift ''QVFFund

-- The stages of a fund over its lifecycle
-- QVFFundActions cause the fund to move through its lifecycle
data QVFFundStage =
    Launched    -- Accepting projects for funding
  | Open        -- Accepting donations
  | Closed      -- Closed to further donations
  | Distributed -- Closed and distributed
    deriving (Eq, P.Show)

PlutusTx.unstableMakeIsData ''QVFFundStage
PlutusTx.makeLift ''QVFFundStage

-- Project descriptor
data Project = Project
  { pAddress   :: !Address                          -- The target of donations
  , pLabel     :: !BuiltinByteString                -- Human readable label
  , pRequested :: !Integer                          -- Amount requested in Lovelace
  , pDonations :: ![(!PaymentPubKeyHash, !Integer)] -- Donations received - TODO make a product type?
  } deriving P.Show

instance Eq QVFFund where
    {-# INLINABLE (==) #-}
    a == b = (pAddress   a == pAddress   b)

PlutusTx.unstableMakeIsData ''Project
PlutusTx.makeLift ''Project

-- The state of the funding round (represented by QVFFundDatum) is updated via the following actions
-- QVFFundActions are passed as redeemers to the validator script
data QVFFundAction =
    -- Add a project for funding. Applicable when stage = Launched (and possibly Open?)
    AddProject !Project
    --  TODO: do we need this?
    -- | RemoveProject Project
    -- Open the fund for donations
  | Open
    -- Make a donation to a project - N.B. The amount is in the UTxO
    -- and the donor can be retrieved via 'ownPaymentPubKeyHash'
  | Donate  !Project
    -- TODO: hah!! But I think automation is soluable with this scheme
    -- | Refund xxx
    -- Close the fund to further donations
  | Close
    -- Distribute the fund to projects per QVF
  | Distribute         -- Distribute the fund to projects per QVF
    deriving P.Show

PlutusTx.unstableMakeIsData ''QVFFundAction
PlutusTx.makeLift ''QVFFundAction

-- The state of the funding round as represented on chain (attached to the script UTxO)
-- This is passed as the Datum to the validator script and represents the current state
-- of the funding round
data QVFFundDatum = QVFFundDatum
    {
      -- The fund to which this data belongs
      fdQVFFund   :: !QVFFund
      -- The stage the fund has reached in its lifecycle
    , fdStage     :: !QVFFundStage
      -- The projects which can be funded - TODO consider using a Set
    , fdProjects  :: ![Project]
      -- TODO: remove. The donations that have been made
      -- , fdDonations :: ![Donation]
    } deriving P.Show

PlutusTx.unstableMakeIsData ''QVFFundDatum
PlutusTx.makeLift ''QVFFundDatum

data QVFFunding
instance Scripts.ValidatorTypes QVFFunding where
    type instance RedeemerType QVFFunding = QVFFundAction
    type instance DatumType QVFFunding = QVFFundDatum

{-# INLINABLE mkQVFFundValidator #-}
mkQVFFundValidator :: QVFFundDatum -> QVFFundAction -> ScriptContext -> Bool
mkQVFFundValidator fundState action ctx =
    case action of
        AddProject p ->
            traceIfFalse "fund not yet accepting projects" $
                         fdStage fundState == Launched &&
            traceIfFalse "project already in fund" $
                         not (p `elem` fdProjects fundState) && -- TODO: Does Plutus have notElem ?
            traceIfFalse "project not in output fund data" $
                         p `elem` fdProjects outputDatum
            -- etc (??)
        Open ->
            traceIfFalse "too early to open fund"
                         correctOpeningTime &&
            traceIfFalse "fund output status not 'Open'" $
                         fdStage outputDatum == Open
            -- etc (??)
        Donate b@Donation{..} ->
            traceIfFalse "donation too low" $
                         sufficientDonation dDonation &&
            traceIfFalse "fund input status not 'Open'" $
                         fdStage fundState == Open &&
            traceIfFalse "project(s) not in fund" $
                         all (`elem` fdProjects fundState) dProjects && -- N.B. True if dProjects == []
            traceIfFalse "donation not in output fund value" $
                         donationInOutputValue dDonation &&
            traceIfFalse "fund output status not 'Open'" $
                         fdStage outputDatum == Open
            -- etc (??)
        Close ->
            traceIfFalse "too early"
                         correctCloseSlotRange &&
            traceIfFalse "fund input status not 'Open'"
                         (fdStage fundState == Open) &&
            -- etc - checkout outputs i.e.
            -- fund value in == fund value out
            -- projects in == projects out
            -- donations in == donations out
        Distribute -> validateDistribution fundState

  where

    validateDistribution :: QVFFundDatum -> ()
    validateDistribution fundState = undefined

    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- N.B. Only one of the Tx inputs is expected to have a Datum and that is the
    -- script output from the previous transaction. Other transaction input data comes
    -- from the redeemer (action)
    input :: TxInInfo
    input =
      let
        isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
            Nothing -> False
            Just _  -> True
        xs = [i | i <- txInfoInputs info, isScriptInput i]
      in
        case xs ofdId       :: !Integer
            [i] -> i
            _   -> traceError "expected exactly one script input"

    qvfFund :: QVFFund
    qvfFund = fdQVFFund ad

    sufficientDonation :: Donation -> Bool
    sufficientDonation d = dDonation d >= fMinDonation ad

    ownOutput   :: TxOut
    outputDatum :: QVFFundDatum
    (ownOutput, outputDatum) = case getContinuingOutputs ctx of
        [o] -> case txOutDatumHash o of
            Nothing   -> traceError "wrong output type"
            Just h -> case findDatum h info of
                Nothing        -> traceError "datum not found"
                Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                    Just ad' -> (o, ad')
                    Nothing  -> traceError "error decoding data"
        _   -> traceError "expected exactly one continuing output"

    correctProjectsOutputValue :: Project -> Bool
    correctProjectsOutputValue p = !(p `elem` fdProjects outputDatum)

    correctOpeningTime :: Bool
    correctOpeningTime = to (fOpeningTime qvfFund) `contains` txInfoValidRange info

    correctDonationSlotRange :: Bool
    correctDonationSlotRange = to (fOpeningTime qvfFund) `contains` txInfoValidRange info

    correctCloseSlotRange :: Bool
    correctCloseSlotRange = from (fClosingTime qvfFund) `contains` txInfoValidRange info

    getsValue :: PaymentPubKeyHash -> Value -> Bool
    getsValue h v =
      let
        [o] = [ o'
              | o' <- txInfoOutputs info
              , txOutValue o' == v
              ]
      in
        txOutAddress o == pubKeyHashAddress h Nothing

typedQVFFundValidator :: Scripts.TypedValidator QVFFunding
typedQVFFundValidator = Scripts.mkTypedValidator @QVFFunding
    $$(PlutusTx.compile [|| mkQVFFundValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @QVFFundDatum @QVFFundAction

qvfFundValidator :: Validator
qvfFundValidator = Scripts.validatorScript typedQVFFundValidator

qvfFundHash :: Ledger.ValidatorHash
qvfFundHash = Scripts.validatorHash typedQVFFundValidator

qvfFundAddress :: Ledger.Address
qvfFundAddress = scriptHashAddress qvfFundHash

-- Off Chain

-- The actions which push a fund through its lifecycle

-- Launch a fund

-- Params required to launch a fund
data LaunchParams = LaunchParams
    {
      -- The fund manager (key holder)
      lpKeyHolder   :: !PaymentPubKeyHash
      -- The time at which the fund opens
    , lpOpeningTime :: !POSIXTime
    -- The time after which refund requests are denied
    -- , lpRefundDeadline :: !POSIXTime
    -- The time the fund closes after which the fund can be distributed
    , lpClosingTime :: !POSIXTime
    -- The minimum donation that can be made - a disencentive to various form of gaming
    , lpMinDonation :: !Integer

    -- The grease that makes it work
    , lpCurrency    :: !CurrencySymbol
    , lpToken       :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

-- Construct and submit the launch transaction
launch :: AsContractError e => LaunchParams -> Contract w s e ()
launch LaunchParams{..} = do
    -- Initialize state of the fund
    pkh <- ownPaymentPubKeyHash
    let -- The fund descriptor
        grease =
            QVFFund
                { fKeyHolder   = pkh
                , fOpeningTime = lpOpeningTime
                , fClosingTime = lpClosingDate
                , fMinDonation = lpMinDonation
                , fCurrency    = lpCurrency
                , fToken       = lpToken
                }
        -- The initial state of the fund
        fundState =
            QVFFundDatum
                { fdQVFFund   = a
                , fdStage    = Launched
                , fdProjects  = []
                -- TODO remove , fdDonations = []
                }
        -- Construct Tx
        -- grease
        v = Value.singleton lpCurrency lpToken 1 <> Ada.lovelaceValueOf minLovelace
        -- fund state d gets attached to script UTxO here
        tx = Constraints.mustPayToTheScript fundState grease

    -- submit Tx
    ledgerTx <- submitTxConstraints typedQVFFundValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

    logInfo @P.String $ printf "launched fund %s for token %s" (P.show a) (P.show v)

-- Add a project to the fund

-- Params required to launch a fund
data AddProjectParams = AddProjectParams
    { apProject  :: !Project
    , apCurrency  :: !CurrencySymbol
    , apToken     :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

-- Construct and submit a transaction to add a project to the fund.
addProject :: forall w s. AddProjectParams -> Contract w s Text ()
addProject AddProjectParams{..} do
    (oref, o, d@QVFFundDatum{..}) <- findQVFFund apCurrency apToken
    -- N.B:  d is now bound to the QVFFundDatum attached to the script UTxO from the previous transaction
    -- so fdProjects take now take an implicit arg d in suitable context
    logInfo @P.String $ printf "found fund utxo with datum %s" (P.show d)

    pkh <- ownPaymentPubKeyHash

    -- carry out checks on params

    -- Only the fund manager can add a project
    ensureFundManager pkh d "only the fund manager can add a project"

    -- Only allow new projects during Launched stage of fund
    when (fdStage != Launched) $
        throwError $ pack $ printf "fund %s is not accepting new projects"
            (P.show $ fundName d)

    -- Don't allow the same project to be added more than once
    when (apProject `elem` fdProjects) $
        throwError $ pack $ printf "project %s already added to fund %s"
            (P.show $ pLabel apProject)
            (P.show $ fundName d)

    -- Set up Tx
    let -- update the state
        newFundState = d {fdProjects = apProject : fdProjects}
        -- construct the action
        action  = Redeemer $ PlutusTx.toBuiltinData $ AddProject apProject
        -- Construct Tx
        grease  = Value.singleton bpCurrency bpToken 1 <> Ada.lovelaceValueOf minLovelace
        lookups = Constraints.typedValidatorLookups typedQVFFundValidator P.<>
                  Constraints.otherScript qvfFundValidator                P.<>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      = Constraints.mustPayToTheScript newFundState grease  <>
                  Constraints.mustSpendScriptOutput oref action

    -- Submit Tx
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

    logInfo @P.String $ printf "adding project %s to fund %s for token (%s, %s)"
        (P.show apProject)
        (P.show fdQVFFund)
        (P.show bpCurrency)
        (P.show bpToken)

fundManager :: QVFFundDatum -> PaymentPubKeyHash
fundManager = fKeyHolder . fdQVFFund

fundName :: :: QVFFundDatum -> BuiltinByteString
fundName = fName . fdQVFFund

ensureFundManager :: PaymentPubKeyHash -> QVFFundDatum -> String -> ()
ensureFundManager pkh d s =  when (pkh != fundManager d) $ throwError $ pack $ printf s

-- Open fund to receive donations

-- Params required to open a fund
data OpenParams = OpenParams
    { opCurrency :: !CurrencySymbol
    , opToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

open :: forall w s. OpenParams -> Contract w s Text ()
open OpenParams{..} = do
    (oref, o, d@QVFFundDatum{..}) <- findQVFFund cpCurrency cpToken
    logInfo @P.String $ printf "found fund utxo with datum %s" (P.show d)

    -- carry out checks on params
    pkh <- ownPaymentPubKeyHash
    ensureFundManager pkh d "only fund manager can open a fund"

    when (fdStage != Launched) $
        throwError $ pack $ printf "fund %s already opened for donations" fdQVFFund

    let -- update the state
        newFundState = d {fdStage = Open}
        -- construct the action
        action  = Redeemer $ PlutusTx.toBuiltinData $ Open
        -- construct Tx
        grease  = Value.singleton opCurrency opToken 1 <> Ada.lovelaceValueOf minLovelace
        lookups = Constraints.typedValidatorLookups typedQVFFundValidator P.<>
                  Constraints.otherScript qvfFundValidator                P.<>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      = Constraints.mustPayToTheScript newFundState grease <>
                  Constraints.mustSpendScriptOutput oref action


    -- submit Tx
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "opened fund %s for token (%s, %s)"
        (P.show fdQVFFund)
        (P.show opCurrency)
        (P.show opToken)

-- Make a donation

-- Params required to donate to a project
data DonationParams = DonationParams
    {
      -- the target of the donation
    , dpProject  :: !Project
    , dpCurrency :: !CurrencySymbol
    , dpToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

donate :: forall w s. DonationParams -> Contract w s Text ()
donate DonationParams{..} = do
    (oref, o, d@QVFFundDatum{..}) <- findQVFFund bpCurrency bpToken
    logInfo @P.String $ printf "found fund utxo with datum %s" (P.show d)

    -- get the amount from the UTxO
    donation <- getDonation

    donor <- ownPaymentPubKeyHash

    -- check params
    when (donation < fdMinDonation) $
        throwError $ pack $ printf "donation %d lower than minimum %d" donation fdMinDonation

    case find (`==` dpProject) fdProjects of
        Nothing ->
            throwError $ pack $ printf "project %s not in fund %s"
                (P.show dpProject)
                (P.show fdQVFFund)
        Just p  ->
            let -- update the projec
                newFundState = updateState donor donation p d

                -- construct the action
                action       = Redeemer $ PlutusTx.toBuiltinData $ Donate p

                -- construct Tx
                grease       = Value.singleton bpCurrency bpToken 1 <> Ada.lovelaceValueOf (minLovelace + donation)

                lookups  = Constraints.typedValidatorLookups typedQVFFundValidator P.<>
                           Constraints.otherScript qvfFundValidator                P.<>
                           Constraints.unspentOutputs (Map.singleton oref o)
                tx       = Constraints.mustPayToTheScript newFundState grease       <>
                           Constraints.mustValidateIn (to $ fClosingTime fdQVFFund) <>
                           Constraints.mustSpendScriptOutput oref action

                -- submit Tx
                ledgerTx <- submitTxConstraintsWith lookups tx
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                logInfo @P.String $ printf "made donation of %d lovelace in fund %s for token (%s, %s)"
                    donation
                    (P.show $ fName fdQVFFund)
                    (P.show bpCurrency)
                    (P.show bpToken)
    where
        updateState :: PaymentPubKeyHash -> Integer -> Project ->
        updateState donor donation p s =
            let -- update the project
                p' = p {pDonations = (donor, donation) : pDonations}

                -- update the fund state with the updated project
                addProj l x = if x == p then p':l else x:l
                projects    = foldl' addProj [] fdProjects
            s {fdProjects = projects}

data CloseParams = CloseParams
    { cpCurrency :: !CurrencySymbol
    , cpToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

close :: forall w s. CloseParams -> Contract w s Text ()
close CloseParams{..} = do
    (oref, o, d@QVFFundDatum{..}) <- findQVFFund cpCurrency cpToken
    logInfo @P.String $ printf "found fund utxo with datum %s" (P.show d)

    let newFundState = d {fdStage = Closed}
        action       = Redeemer $ PlutusTx.toBuiltinData Close
        grease       = Value.singleton cpCurrency cpToken 1
        lookups = Constraints.typedValidatorLookups typedQVFFundValidator P.<>
                  Constraints.otherScript qvfFundValidator                P.<>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      = Constraints.mustPayToTheScript newFundState grease
                  Constraints.mustSpendScriptOutput oref action

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "closed fund %s for token (%s, %s)"
        (P.show fdQVFFund)
        (P.show cpCurrency)
        (P.show cpToken)

data DistributeParams = DistributeParams
    { distKeyHolder :: !PaymentPubKeyHash
    , distCurrency :: !CurrencySymbol
    , distToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

distribute :: forall w s. DistributeParams -> Contract w s Text ()
distribute DistributeParams{..} = do
    let newFundState = d {fdStage = Closed}
        action       = Redeemer $ PlutusTx.toBuiltinData Close
        grease       = Value.singleton cpCurrency cpToken 1
        lookups = Constraints.typedValidatorLookups typedQVFFundValidator P.<>
                  Constraints.otherScript qvfFundValidator                P.<>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      = Constraints.mustPayToTheScript newFundState grease <>
                  Constraints.mustSpendScriptOutput oref action
        tx'     = allocateFunds tx d

    ledgerTx <- submitTxConstraintsWith lookups tx'
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "closed fund %s for token (%s, %s)"
        (P.show fdQVFFund)
        (P.show cpCurrency)
        (P.show cpToken)
    where
        allocateFunds tx state =
            -- here's where we do the QVF calculation
            -- we return an update set of constraints which include
            -- Constraints.mustPayToPubKey project (Ada.lovelaceValueOf allocation)
            undefined

findQVFFund :: CurrencySymbol
            -> TokenName
            -> Contract w s Text (TxOutRef, ChainIndexTxOut, QVFFundDatum)
findQVFFund cs tn = do
    utxos <- utxosAt $ scriptHashAddress qvfFundHash
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxosdId       :: !Integer
    case xs of
        [(oref, o)] -> case _ciTxOutDatum o of
            Left _          -> throwError "datum missing"
            Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> throwError "datum has wrong type"
                Just d@QVFFundDatum{..}
                    | fCurrency fdQVFFund == cs && fToken fdQVFFund == tn -> return (oref, o, d)
                    | otherwise                                           -> throwError "fund token missmatch"
        _           -> throwError "fund utxo not found"

-- Corresponds to redeemers + launch
type QVFFundSchema =
        Endpoint "launch" LaunchParams
    .\/ Endpoint "add project" AddProjectParams
    .\/ Endpoint "donate"   DonationParams
    .\/ Endpoint "close"   CloseParams
    .\/ Endpoint "distribute" DistributeParams

endpoints :: Contract () QVFFundSchema Text ()
endpoints = awaitPromise (launch' `select` addProject' `select` open' `select` donate' `select` close' `select` distribute') >> endpoints
  where
    launch'     = endpoint @"launch" launch
    addProject  = endpoint @"addProject" addProject
    open'       = endpoint @"open" open
    donate'     = endpoint @"donate"   donate
    close'      = endpoint @"close" close
    distribute' = endpoint @"distribute" distribute

mkSchemaDefinitions ''QVFFundSchema

myToken :: KnownCurrency
myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])

mkKnownCurrencies ['myToken]
