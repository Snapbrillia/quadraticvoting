{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE NumericUnderscores #-}

module CLI.Tx where


-- IMPORTS
-- {{{
import qualified Control.Monad.Fail          as M
import qualified Data.Aeson                  as A
import           Data.Aeson                  ( FromJSON(..)
                                             , (.:)
                                             , (.:?) )
import           Data.Aeson.Types            ( Parser )
import qualified Data.List                   as List
import           Data.Text                   ( Text )
import qualified Data.Text                   as Text
import qualified Ledger.Ada                  as Ada
import qualified Plutus.V1.Ledger.Value      as Value
import           Plutus.V1.Ledger.Value      ( CurrencySymbol(..)
                                             , TokenName(..) )
import           Plutus.V2.Ledger.Api        ( TxOutRef(..)
                                             , TxInInfo(..)
                                             , TxOut(..) )
import qualified Plutus.V2.Ledger.Api        as Ledger
import qualified PlutusTx
import qualified PlutusTx.AssocMap           as Map
import           PlutusTx.AssocMap           ( Map )
import           Prelude

import           CLI.Utils
import           Data.Datum
import qualified QVF                         as OC
import           Utils
-- }}}


-- ASSET
-- {{{
data Asset = Asset
  { assetSymbol    :: CurrencySymbol
  , assetTokenName :: TokenName
  } deriving (Eq)

instance Ord Asset where
  -- {{{
  compare a0 a1 =
    case compare (assetSymbol a0) (assetSymbol a1) of
      EQ -> compare (assetTokenName a0) (assetTokenName a1)
      o  -> o
  -- }}}

instance Show Asset where
  -- {{{
  show a =
    let
      CurrencySymbol sym = assetSymbol a
      TokenName      tn  = assetTokenName a
    in
    builtinByteStringToString sym ++
      (if tn == "" then "" else "." ++ builtinByteStringToString tn)
  -- }}}

readAsset :: String -> Maybe Asset
readAsset a =
  -- {{{
  case span (/= '.') a of
    (sym, [])     -> do
      bbs <- hexStringToBuiltinByteString sym
      return $ Asset (CurrencySymbol bbs) emptyTokenName
    (sym, _ : tn) -> do
      symBBS <- hexStringToBuiltinByteString sym
      tnBBS  <- hexStringToBuiltinByteString tn
      return $ Asset (CurrencySymbol symBBS) (TokenName tnBBS)
  -- }}}
-- }}}


-- TX OUTPUT
-- {{{
data Output = Output
  { oAddress    :: Ledger.Address
  , oAddressStr :: String
  , oLovelace   :: Integer
  , oForScript  :: Maybe (Asset, Integer, QVFDatum)
  } deriving (Eq)

instance Show Output where
  show Output{..} =
    -- {{{
       "{\"address\":"    ++ "\"" ++      oAddressStr ++ "\""
    ++ ",\"lovelace\":"   ++         show oLovelace
    ++ ( case oForScript of
           Just (oAsset, oAssetCount, oDatum) ->
             -- {{{
                ",\"asset\":"      ++ "\"" ++        show oAsset      ++ "\""
             ++ ",\"assetCount\":" ++                show oAssetCount
             ++ ",\"datumCBOR\":"  ++ "\"" ++ typedToCBOR oDatum      ++ "\""
             -- }}}
           Nothing                            ->
             -- {{{
             ""
             -- }}}
       )
    ++ "}"
    -- }}}
-- }}}


-- TX INPUT
-- {{{
data Input = Input
  { iTxOutRef :: TxOutRef
  , iResolved :: Output
  } deriving (Eq)

instance Show Input where
  show Input{..} =
       init (show iResolved) -- WARNING: Used `init` with caution.
    ++ ",\"utxo\":" ++ "\"" ++ showTxOutRef iTxOutRef ++ "\""
    ++ "}"

instance FromJSON Input where
  -- parseJSON :: Value -> Parser Input
  parseJSON = A.withObject "Input" $ \v -> do
    -- {{{
    initUTxO   <- v .:  "utxo"
    initAddr   <- v .:  "address"
    mInitAsset <- v .:? "asset"
    mInitCBOR  <- v .:? "datumCBOR"
    case initUTxO of
      A.String utxo ->
        -- {{{
        case initAddr of
          A.String addr ->
            -- {{{
            let
              fromOut out = Input <$> utxoParser utxo <*> out
            in
            case (mInitAsset, mInitCBOR) of
              (Just (A.String asset), Just (A.String cbor)) ->
                -- {{{
                let
                  forScript =
                    -- {{{
                        (,,)
                    <$> assetParser asset
                    <*> v .: "assetCount"
                    <*> qvfDatumParser cbor
                    -- }}}
                  out       =
                    -- {{{
                        Output
                    <$> addressParser addr
                    <*> v .: "address"
                    <*> v .: "lovelace"
                    <*> (Just <$> forScript)
                    -- }}}
                in
                fromOut out
                -- }}}
              (Nothing              , Nothing             ) ->
                -- {{{
                let
                  out =
                    -- {{{
                        Output
                    <$> addressParser addr
                    <*> v .: "address"
                    <*> v .: "lovelace"
                    <*> return Nothing
                    -- }}}
                in
                fromOut out
                -- }}}
              _                                             ->
                -- {{{
                M.fail "Invalid asset and/or CBOR."
                -- }}}
            -- }}}
          _             ->
            -- {{{
            M.fail "Invalid address."
            -- }}}
        -- }}}
      _             ->
        -- {{{
        M.fail "Invalid UTxO."
        -- }}}
    -- }}}
-- }}}


-- UTILS
-- {{{
showTxOutRef :: TxOutRef -> String
showTxOutRef TxOutRef{..} =
  -- {{{
     builtinByteStringToString (Ledger.getTxId txOutRefId)
  ++ "#"
  ++ show txOutRefIdx
  -- }}}


utxoParser :: Text -> Parser TxOutRef
utxoParser txt =
  -- {{{
  case readTxOutRef (Text.unpack txt) of
    Just utxo ->
      return utxo
    Nothing   ->
      M.fail "Invalid TxOutRef."
  -- }}}


assetParser :: Text -> Parser Asset
assetParser txt =
  -- {{{
  case readAsset (Text.unpack txt) of
    Just a  ->
      return a
    Nothing ->
      M.fail "Invalid Asset."
  -- }}}


addressParser :: Text -> Parser Ledger.Address
addressParser txt =
  -- {{{
  case tryReadAddress txt of
    Just addr ->
      return addr
    Nothing   ->
      M.fail "Invalid Address."
  -- }}}


qvfDatumParser :: Text -> Parser QVFDatum
qvfDatumParser txt =
  -- {{{
  case cborStringToData (Text.unpack txt) of
    Just d  ->
      case PlutusTx.fromData d of
        Just qvfD ->
          return qvfD
        Nothing   ->
          M.fail "Can't convert Data to QVFDatum"
    Nothing ->
      M.fail "Invalid Data."
  -- }}}


inputToTxInInfo :: Input -> TxInInfo
inputToTxInInfo Input{..} =
  -- {{{
  TxInInfo iTxOutRef $ outputToTxOut iResolved
  -- }}}


outputToTxOut :: Output -> TxOut
outputToTxOut Output{..} =
  -- {{{
  let
    (extraVal, d) =
      case oForScript of
        Just (Asset{..}, assetCount, qvfD) ->
          -- {{{
          ( Value.singleton assetSymbol assetTokenName assetCount
          , qvfDatumToInlineDatum qvfD
          )
          -- }}}
        Nothing                            ->
          -- {{{
          (mempty, Ledger.NoOutputDatum)
          -- }}}
  in
  TxOut oAddress (Ada.lovelaceValueOf oLovelace <> extraVal) d Nothing
  -- }}}


txOutToOutput :: String -> TxOut -> Maybe Output
txOutToOutput addrStr o@TxOut{..} = do
  -- {{{
  inAddr <- tryReadAddress $ Text.pack addrStr
  if inAddr == txOutAddress then
    -- {{{
    case Value.flattenValue txOutValue of
      [(sym', tn', amt'), (_, _, lovelaces)] ->
        -- {{{
        Just $ Output
          { oAddress    = txOutAddress
          , oAddressStr = addrStr
          , oLovelace   = lovelaces
          , oForScript  = Just (Asset sym' tn', amt', getInlineDatum o)
          }
        -- }}}
      [(_, _, lovelaces)]                    ->
        -- {{{
        Just $ Output
          { oAddress    = txOutAddress
          , oAddressStr = addrStr
          , oLovelace   = lovelaces
          , oForScript  = Nothing
          }
        -- }}}
      _                                      ->
        -- {{{
        Nothing
        -- }}}
    -- }}}
  else
    -- {{{
    Nothing
    -- }}}
  -- }}}


getAssetFromInput :: Input -> Maybe Asset
getAssetFromInput i =
  -- {{{
  case i of
    Input{iResolved = Output{oForScript = Just (a, _, _)}} -> Just a
    _                                                      -> Nothing
  -- }}}


compareInputs :: Input -> Input -> Ordering
compareInputs
  Input{iTxOutRef = r0, iResolved = Output{oForScript = mT0}}
  Input{iTxOutRef = r1, iResolved = Output{oForScript = mT1}} =
  -- {{{
  case (mT0, mT1) of
    (Just (a0, _, _), Just (a1, _, _)) ->
      compare a0 a1
    (Just (_, _, _) , Nothing        ) ->
      GT
    (Nothing        , Just (_, _, _) ) ->
      LT
    (Nothing        , Nothing        ) ->
      compare r0 r1
  -- }}}
-- }}}


-- IO
-- {{{
-- | Attempts decoding given strings into `Input` values, and applies the
--   given IO action in case of success.
fromGovAndInputs :: String
                 -> String
                 -> Maybe String
                 -> (Input -> [Input] -> [Input] -> IO ())
                 -> IO ()
fromGovAndInputs govInputStr inputsStr mRefsStr action =
  -- {{{
  let
    mGov    = decodeString @Input   govInputStr
    mInputs = decodeString @[Input] inputsStr
  in
  case mRefsStr of
    Just refsStr ->
      -- {{{
      case (mGov, mInputs, decodeString @[Input] refsStr) of
        (Just govInput, Just inputs, Just refs) ->
          action govInput inputs refs
        _                                       ->
          putStrLn $ "FAILED: Bad arguments:"
            ++ "\n\t" ++ govInputStr
            ++ "\n\t" ++ inputsStr
            ++ "\n\t" ++ refsStr
      -- }}}
    Nothing      ->
      -- {{{
      case (mGov, mInputs) of
        (Just govInput, Just inputs) ->
          action govInput inputs []
        _                            ->
          putStrLn $ "FAILED: Bad arguments:"
            ++ "\n\t" ++ govInputStr
            ++ "\n\t" ++ inputsStr
      -- }}}
  -- }}}
-- }}}


-- SCRIPT INPUT/OUTPUT
-- {{{
data ScriptOutput = ScriptOutput
  { soAddress    :: Ledger.Address
  , soAddressStr :: String
  , soLovelace   :: Integer
  , soAsset      :: Asset
  , soAssetCount :: Integer
  , soDatum      :: QVFDatum
  } deriving (Eq)

data ScriptInput = ScriptInput
  { siTxOutRef :: TxOutRef
  , siResolved :: ScriptOutput
  } deriving (Eq)


inputToScriptInput :: Input -> Maybe ScriptInput
inputToScriptInput Input{..} = do
  -- {{{
  resolved <- outputToScriptOutput iResolved
  return $ ScriptInput
    { siTxOutRef = iTxOutRef
    , siResolved = resolved
    }
  -- }}}


inputToAuthenticScriptInput :: [CurrencySymbol]
                            -> Input
                            -> Maybe ScriptInput
inputToAuthenticScriptInput authenticSymbols Input{..} = do
  -- {{{
  resolved <- outputToAuthenticScriputOutput authenticSymbols iResolved
  return $ ScriptInput
    { siTxOutRef = iTxOutRef
    , siResolved = resolved
    }
  -- }}}


outputToScriptOutput :: Output -> Maybe ScriptOutput
outputToScriptOutput Output{..} = do
  -- {{{
  (a, c, d) <- oForScript
  return $ ScriptOutput
    { soAddress    = oAddress
    , soAddressStr = oAddressStr
    , soLovelace   = oLovelace
    , soAsset      = a
    , soAssetCount = c
    , soDatum      = d
    }
  -- }}}


outputToAuthenticScriputOutput :: [CurrencySymbol]
                               -> Output
                               -> Maybe ScriptOutput
outputToAuthenticScriputOutput authenticSymbols o = do
  -- {{{
  so@ScriptOutput{..} <- outputToScriptOutput o
  if elem (assetSymbol soAsset) authenticSymbols then
    return so
  else
    Nothing
  -- }}}


scriptOutputToOutput :: ScriptOutput -> Output
scriptOutputToOutput ScriptOutput{..} =
  -- {{{
  Output
    { oAddress    = soAddress
    , oAddressStr = soAddressStr
    , oLovelace   = soLovelace
    , oForScript  = Just (soAsset, soAssetCount, soDatum)
    }
  -- }}}


scriptInputToInput :: ScriptInput -> Input
scriptInputToInput ScriptInput{..} =
  -- {{{
  Input
    { iTxOutRef = siTxOutRef
    , iResolved = scriptOutputToOutput siResolved
    }
  -- }}}


compareScriptOutputs :: ScriptOutput -> ScriptOutput -> Ordering
compareScriptOutputs ScriptOutput{soAsset = a0} ScriptOutput{soAsset = a1} =
  -- {{{
  compare a0 a1
  -- }}}
-- }}}


-- SPECIFIC OUTPUTS
-- {{{
data DeadlineOutput = DeadlineOutput
  { doDeadline :: Ledger.POSIXTime
  , doSymbol   :: CurrencySymbol
  } deriving (Eq)

data MatchPoolOutput = MatchPoolOutput
  { mpoMatchPool :: Integer
  , mpoSymbol    :: CurrencySymbol
  } deriving (Eq)

-- ^ Note that as this set of datatypes are only meant to act as intermediary
--   constructs, the double-source-of-truth approach (total raised donations)
--   has been carefully implemented in favor of performance.
data ProjectStateOutput = ProjectStateOutput
  { psoLovelace    :: Integer
  , psoSymbol      :: CurrencySymbol
  , psoTokenName   :: TokenName
  , psoDonations   :: Map Ledger.PubKeyHash Integer
  , psoTotalRaised :: Integer
  } deriving (Eq)

data ProjectInfoOutput = ProjectInfoOutput
  { pioLovelace  :: Integer
  , pioSymbol    :: CurrencySymbol
  , pioTokenName :: TokenName
  , pioDetails   :: ProjectDetails
  } deriving (Eq)

instance Show ProjectStateOutput where
  show ProjectStateOutput{..} =
    -- {{{
    let
      showPair (k, v) =
           "{\"" ++ builtinByteStringToString (Ledger.getPubKeyHash k) ++ "\":" 
        ++ show v
        ++ "}"
    in
       "{\"" ++ unsafeTokenNameToHex psoTokenName ++ "\":["
    ++ List.intercalate "," (showPair <$> Map.toList psoDonations)
    ++ "]"
    ++ ",\"total\":" ++ show psoTotalRaised
    ++ "}"
    -- }}}

data DonationOutput = DonationOutput
  { doDonor     :: Ledger.PubKeyHash
  , doDonation  :: Integer
  , doTokenName :: TokenName
  } deriving (Eq)
-- }}}


-- DISTRIBUTION INFO
-- {{{
data DistributionInfo = DistributionInfo
  { diProjectID         :: Ledger.BuiltinByteString
  , diRequested         :: Integer
  , diRaised            :: Integer
  , diRaisedAfterFee    :: Integer
  , diMatchPool         :: Integer
  , diMatchPoolAfterFee :: Integer
  , diPrizeWeight       :: Integer
  , diRatioNum          :: Integer
  , diRatioDen          :: Integer
  } deriving (Eq)

instance Show DistributionInfo where
  show DistributionInfo{..} =
    -- {{{
    let
      theRatio :: Double
      theRatio = fromIntegral diRatioNum / fromIntegral diRatioDen
    in
       "{\"tn\":"                ++ "\"" ++ builtinByteStringToString diProjectID ++ "\""
    ++ ",\"requested\":"         ++ show diRequested
    ++ ",\"raised\":"            ++ show diRaised
    ++ ",\"raisedAfterFee\":"    ++ show diRaisedAfterFee
    ++ ",\"matchPool\":"         ++ show diMatchPool
    ++ ",\"matchPoolAfterFee\":" ++ show diMatchPoolAfterFee
    ++ ",\"prizeWeight\":"       ++ show diPrizeWeight
    ++ ",\"ratio\":"             ++ show theRatio
    ++ "}"
  -- }}}

prettyDistributionInfo :: DistributionInfo -> String
prettyDistributionInfo DistributionInfo{..} =
  -- {{{
  let
    fI             = fromIntegral
    showAda ls     = show (fI ls / 1_000_000)
    toPercent      :: Integral a => Double -> a
    toPercent      = min 100 . round . (100 *)
    ratioPercent   = toPercent (fI diRatioNum / fI diRatioDen)
    raisedPercent  = toPercent (fI diRaised / fI diRequested)
    mpPercent      = max 0 (ratioPercent - raisedPercent)
    raisedChars    = replicate raisedPercent        '═'
    mpChars        = replicate mpPercent            '═'
    spaces         = replicate (100 - ratioPercent) ' '
    isEliminated   = ratioPercent < 100
    labelDen       = showAda diRequested
    mainColor      = if isEliminated then red else green
    labelNum       =
      if isEliminated then
        showAda diRaisedAfterFee
      else
           "(" ++ showAda diRaisedAfterFee
        ++ purple ++ " + " ++ showAda diMatchPoolAfterFee ++ mainColor
        ++ ")"
  in
     mainColor
  ++ builtinByteStringToString diProjectID ++ ":\t"
  ++ "[" ++ raisedChars ++ purple ++ mpChars ++ mainColor ++ spaces ++ "]"
  ++ "\t" ++ labelNum ++ " / " ++ labelDen
  ++ noColor
  -- }}}

eliminationInfoToDistributionInfo :: Integer
                                  -> Integer
                                  -> (Ledger.BuiltinByteString, EliminationInfo)
                                  -> DistributionInfo
eliminationInfoToDistributionInfo matchPool sumW (tn, ei) =
  let
    mpPortion = findMatchPoolPortion matchPool sumW (eiWeight ei)
  in
  DistributionInfo
    { diProjectID         = tn
    , diRequested         = eiRequested ei
    , diRaised            = eiRaised ei
    , diRaisedAfterFee    = snd $ OC.separateKeyHoldersFeeFrom $ eiRaised ei
    , diMatchPool         = mpPortion
    , diMatchPoolAfterFee = snd $ OC.separateKeyHoldersFeeFrom mpPortion
    , diPrizeWeight       = eiWeight ei
    , diRatioNum          = findFundedToRequestedRatio matchPool sumW ei
    , diRatioDen          = decimalMultiplier
    }
-- }}}


-- EMULATION RESULT
-- {{{
data EmulationResult = EmulationResult
  { erDistributionInfos :: [DistributionInfo]
  , erInputUTxOs        :: [TxOutRef]
  } deriving (Eq)

instance Show EmulationResult where
  show EmulationResult{..} =
       "{\"infos\":"  ++ show erDistributionInfos
    ++ ",\"inputs\":" ++ show (showTxOutRef <$> erInputUTxOs)
    ++ "}"
-- }}}

