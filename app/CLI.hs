{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}


module Main (main) where


import           Cardano.Api
import           Cardano.Api.Shelley        (PlutusScript (..))
import           Codec.Serialise            (Serialise, serialise)
import qualified Data.Aeson                 as A
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Short      as SBS
import qualified Data.List                  as List
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromJust)
import           Data.String                (fromString)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Plutus.V1.Ledger.Api       (fromBuiltin, BuiltinByteString)
import           Plutus.V1.Ledger.Value     (TokenName (..))
import           PlutusTx                   (Data (..))
import qualified PlutusTx
import qualified Ledger
import           System.Environment         (getArgs)
import           Text.Read                  (readMaybe)

import qualified OnChain                    as OC
import qualified Token

-- UTILS
-- {{{
dataToScriptData :: Data -> ScriptData
-- {{{
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs
-- }}}


scriptDataToData :: ScriptData -> Data
  -- {{{
scriptDataToData (ScriptDataConstructor n xs) =
  Constr n $ scriptDataToData <$> xs
scriptDataToData (ScriptDataMap xs)           =
  Map [(scriptDataToData x, scriptDataToData y) | (x, y) <- xs]
scriptDataToData (ScriptDataList xs)          =
  List $ scriptDataToData <$> xs
scriptDataToData (ScriptDataNumber n)         =
  I n
scriptDataToData (ScriptDataBytes bs)         =
  B bs
  -- }}}


writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file =
  -- {{{
    LBS.writeFile file
  . encode
  -- . scriptDataToJson ScriptDataJsonNoSchema
  . scriptDataToJson ScriptDataJsonDetailedSchema
  . dataToScriptData
  . PlutusTx.toData
  -- }}}


parseJSONValue :: LBS.ByteString -> IO (Either String Data)
parseJSONValue bs =
  -- {{{
  case A.decode bs of
    Just decoded ->
      case scriptDataFromJson ScriptDataJsonDetailedSchema decoded of
        Right scriptData ->
          return $ Right (scriptDataToData scriptData)
        Left err ->
          return $ Left $ show err
    Nothing ->
      return $ Left "Invalid JSON."
  -- }}}


-- TODO !@! - remove? Not used.
-- parseJSON :: FilePath -> IO (Either String Data)
-- parseJSON file = do
--   -- {{{
--   fileContent <- LBS.readFile file
--   parseJSONValue fileContent
--   -- }}}


writeScript :: Serialise a => FilePath -> a -> IO (Either (FileError ()) ())
writeScript file =
  -- {{{
    writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing
  . PlutusScriptSerialised
  . SBS.toShort
  . LBS.toStrict
  . serialise
  -- }}}


writeValidator :: FilePath
               -> Ledger.Validator
               -> IO (Either (FileError ()) ())
writeValidator file =
  -- {{{
  writeScript file . Ledger.unValidatorScript
  -- }}}


writeMintingPolicy :: FilePath
                   -> Ledger.MintingPolicy
                   -> IO (Either (FileError ()) ())
writeMintingPolicy file =
  -- {{{
  writeScript file . Ledger.getMintingPolicy
  -- }}}


readTxOutRef :: String -> Maybe Ledger.TxOutRef
readTxOutRef s =
  -- {{{
  case span (/= '#') s of
    (x, _ : y) ->
      -- {{{
      Just $ Ledger.TxOutRef
        { Ledger.txOutRefId  = fromString x
        , Ledger.txOutRefIdx = read y
        }
      -- }}}
    _          ->
      -- {{{
      Nothing
      -- }}}
  -- }}}

builtinByteStringToString :: BuiltinByteString -> String
builtinByteStringToString =
    BS8.unpack
  . serialiseToRawBytesHex
  . fromJust
  . deserialiseFromRawBytes AsAssetName
  . fromBuiltin

-- | From PPP's 6th lecture.
unsafeTokenNameToHex :: TokenName -> String
unsafeTokenNameToHex =
  -- {{{
  builtinByteStringToString . unTokenName
  -- }}}
-- }}}


-- APPLICATION
-- {{{
main :: IO ()
main =
  let
    -- {{{
    makeHelpText :: String -> String -> String -> [String] -> String
    makeHelpText description elem0 elem1 restOfElems =
      -- {{{
      let
        elems           = elem0 : elem1 : restOfElems
        cmd             :: String
        cmd             = "qvf-cli "
        makeBlank x     = replicate x ' '
        preBlank        :: String
        preBlank        = "\t" ++ makeBlank (length cmd)
        longest         =
          -- {{{
          length $ List.maximumBy
            (\arg0 arg1 -> compare (length arg0) (length arg1))
            elems
          -- }}}
        withPostBlank e = e ++ makeBlank (longest - length e) ++ " \\\n"
        elemsWithBlanks = map ((preBlank ++) . withPostBlank) $ tail $ init elems
      in
         "\n\n\t" ++ description ++ "\n\n"
      ++ "\t" ++ cmd
      ++ withPostBlank elem0
      ++ concat elemsWithBlanks
      ++ preBlank ++ last elems ++ "\n"
      -- }}}

    -- SUB-HELP TEXTS: 
    -- {{{
    helpCurrDatum      :: String
    helpCurrDatum      = "<current.datum>"
    helpUpdatedDatum   :: String
    helpUpdatedDatum   = "<updated.datum>"
    helpRedeemer       :: String
    helpRedeemer       = "<action.redeemer>"
    commonLastThree    = [helpCurrDatum, helpUpdatedDatum, helpRedeemer]
    toHexHelp          :: String
    toHexHelp          =
      -- {{{
      makeHelpText
        "Export the hex serialization of a token name:"
        "string-to-hex"
        "<token-name>"
        ["<output.hex>"]
      -- }}}
    prettyDatumHelp    :: String
    prettyDatumHelp    =
      -- {{{
      makeHelpText
        "Print an easy-to-read parsing of a given datum JSON:"
        "pretty-datum"
        "{current-datum-json-value}"
        []
      -- }}}
    dataToCBORHelp     :: String
    dataToCBORHelp     =
      -- {{{
      makeHelpText
        "Return the CBOR encoding of a given JSON formatted `Data` value:"
        "data-to-cbor"
        "{arbitrary-data-json-value}"
        []
      -- }}}
    scriptHelp         :: String
    scriptHelp         =
      -- {{{
      makeHelpText
        (    "Generate the compiled Plutus validation and minting script\n"
          ++ "\t(note the UTxO format):"
        )
        "generate"
        "scripts"
        [ "<key-holder-pub-key-hash>"
        , "<txID>#<output-index>"
        , "<auth-token-name>"
        , "<deadline-posix-milliseconds>"
        , "<output-minting.plutus>"
        , "<output-validation.plutus>"
        , "<unit.redeemer>"
        , "<output-initial.datum>"
        ]
      -- }}}
    addProjectHelp     :: String
    addProjectHelp     =
      -- {{{
      makeHelpText
       "Update a given datum by adding a project:"
       "add-project"
       "<project-public-key-hash>" $
       [ "<project-label>"
       , "<requested-fund>"
       ] ++ commonLastThree
      -- }}}
    donateHelp         :: String
    donateHelp         =
      -- {{{
      makeHelpText
        "Update a given datum by donating to a list of projects:"
        "donate"
        "<donors-public-key-hash>" $
        [ "<projects-pubkeyhash-0>"
        , "<donation-amount-----0>"
        , "<projects-pubkeyhash-1>"
        , "<donation-amount-----1>"
        , "<projects-pubkeyhash-2>"
        , "<donation-amount-----2>"
        , "<...etc...>"
        ] ++ commonLastThree
      -- }}}
    contributeHelp     :: String
    contributeHelp     =
      -- {{{
      makeHelpText
        "Update a given datum by contributing to the pool:"
        "contribute"
        "<contribution-amount>"
        commonLastThree
      -- }}}
    setDeadlineHelp    :: String
    setDeadlineHelp    =
      -- {{{
      makeHelpText
        "Update a given datum by setting a new deadline:"
        "set-deadline"
        "<new-deadline>"
        commonLastThree
      -- }}}
    distributeHelp     :: String
    distributeHelp     =
      -- {{{
      makeHelpText
        (    "Generate `--tx-out` arguments to pass to the `cardano-cli`\n"
          ++ "\tapplication, plus the updated datum and redeemer files:"
        )
        "distribute"
        "<key-holder-address>" $
        [ "<projects-pubkeyhash-0>"
        , "<projects-address----0>"
        , "<projects-pubkeyhash-1>"
        , "<projects-address----1>"
        , "<projects-pubkeyhash-2>"
        , "<projects-address----2>"
        , "<...etc...>"
        ] ++ commonLastThree
      -- }}}
    emulateDistHelp    :: String
    emulateDistHelp    =
      -- {{{
      makeHelpText
        (    "Print a JSON of the prizes each project receives up to\n"
          ++ "\tthis point:"
        )
        "emulate-distribution"
        "{target-datum-json-value}"
        []
      -- }}}
    donationImpactHelp :: String
    donationImpactHelp =
      -- {{{
      makeHelpText
        (    "Print a JSON of the changes in received prize amounts after\n"
          ++ "\ta donation:"
        )
        "donation-impact"
        "<donors-public-key-hash>"
        [ "<projects-pubkeyhash-0>"
        , "<donation-amount-----0>"
        , "<projects-pubkeyhash-1>"
        , "<donation-amount-----1>"
        , "<projects-pubkeyhash-2>"
        , "<donation-amount-----2>"
        , "<...etc...>"
        , "{target-datum-json-value}"
        ]
      -- }}}
    deadlineToSlotHelp :: String
    deadlineToSlotHelp =
      -- {{{
      makeHelpText
        "Convert the deadline in a given datum to the slot number:"
        "get-deadline-slot"
        "<current-slot-number>"
        [helpCurrDatum]
      -- }}}
    -- }}}

    -- GENERIC HELP TEXT: 
    helpText          :: String
    helpText          =
      -- {{{
         "\nCLI application to generate various redeemer values to interact\n"
      ++ "with the QVF smart contract.\n\n"

      ++ "You can separately print the argument guide for each action\n"
      ++ "with (-h|--help|man) following the desired action. Available\n"
      ++ "options are:\n\n"

      ++ "Primary interactions:\n"
      ++ "\tqvf-cli add-project --help\n"
      ++ "\tqvf-cli donate      --help\n"
      ++ "\tqvf-cli contribute  --help\n\n"

      ++ "Utility:\n"
      ++ "\tqvf-cli generate scripts     --help\n"
      ++ "\tqvf-cli pretty-datum         --help\n"
      ++ "\tqvf-cli data-to-cbor         --help\n"
      ++ "\tqvf-cli string-to-hex        --help\n"
      ++ "\tqvf-cli get-deadline-slot    --help\n"
      ++ "\tqvf-cli emulate-distribution --help\n"
      ++ "\tqvf-cli donation-impact      --help\n\n"

      ++ "Limited to key holder:\n"
      ++ "\tqvf-cli set-deadline --help\n"
      ++ "\tqvf-cli distribute   --help\n\n"

      ++ "Or simply use (-h|--help|man) to print this help text.\n\n"
      -- }}}

    printHelp         :: IO ()
    printHelp         = putStrLn helpText

    andPrintSuccess :: FilePath -> IO () -> IO ()
    andPrintSuccess outFile ioAction = do
      -- {{{
      ioAction
      putStrLn $ outFile ++ " generated SUCCESSFULLY."
      -- }}}

    fromAction :: Bool
               -> OC.QVFAction
               -> OC.QVFDatum
               -> Maybe String
               -> String
               -> IO ()
    fromAction verbose action currDatum mDOF rOF =
      -- {{{
      case OC.updateDatum action currDatum of
        Left err       ->
          -- {{{
          putStrLn $ "FAILED: Bad redeemer, " ++ show err
          -- }}}
        Right newDatum ->
          -- {{{
          let
            writerHelper outFile dataToWrite =
              if verbose then
                andPrintSuccess outFile $ writeJSON outFile dataToWrite
              else
                writeJSON outFile dataToWrite
            writeRedeemer = writerHelper rOF action
          in
          case mDOF of
            Just dOF -> do
              writerHelper dOF newDatum
              writeRedeemer
            Nothing  ->
              writeRedeemer
          -- }}}
      -- }}}

    printGenerateHelp :: String -> IO ()
    printGenerateHelp genStr =
      -- {{{
      case genStr of
        "scripts"               ->
          putStrLn scriptHelp
        _                       ->
          printHelp
      -- }}}

    printActionHelp :: String -> IO ()
    printActionHelp action =
      -- {{{
      case action of
        "add-project"          ->
          putStrLn addProjectHelp
        "donate"               ->
          putStrLn donateHelp
        "contribute"           ->
          putStrLn contributeHelp
        "set-deadline"         ->
          putStrLn setDeadlineHelp
        "distribute"           ->
          putStrLn distributeHelp
        "pretty-datum"         ->
          putStrLn prettyDatumHelp
        "data-to-cbor"         ->
          putStrLn dataToCBORHelp
        "string-to-hex"        ->
          putStrLn toHexHelp
        "distribute"           ->
          putStrLn distributeHelp
        "emulate-distribution" ->
          putStrLn emulateDistHelp
        "donation-impact"      ->
          putStrLn donationImpactHelp
        "get-deadline-slot"    ->
          putStrLn deadlineToSlotHelp
        _                      ->
          printHelp
      -- }}}

    fromConcreteDataValue :: LBS.ByteString
                          -> (String -> IO a)
                          -> (Data -> IO a)
                          -> IO a
    fromConcreteDataValue datVal parseFailureAction dataToIO = do
      -- {{{
      eitherErrData <- parseJSONValue datVal
      case eitherErrData of
        Left parseError ->
          -- {{{
          parseFailureAction parseError
          -- }}}
        Right dataData ->
          -- {{{
          dataToIO dataData
          -- }}}
      -- }}}

    fromDatumValueHelper :: LBS.ByteString
                         -> (String -> IO a)
                         -> IO a
                         -> (OC.QVFDatum -> IO a)
                         -> IO a
    fromDatumValueHelper datVal parseFailureAction badDataAction dataToIO =
      -- {{{
      fromConcreteDataValue datVal parseFailureAction $ \dataData ->
        maybe badDataAction dataToIO $ PlutusTx.fromData dataData
      -- }}}

    fromDatumValue :: LBS.ByteString
                   -> (OC.QVFDatum -> IO ())
                   -> IO ()
    fromDatumValue datVal =
      -- {{{
      fromDatumValueHelper
        datVal
        ( \parseError ->
            putStrLn $ "FAILED to parse data JSON: " ++ parseError
        )
        (putStrLn "FAILED: Improper data.")
      -- }}}

    actOnData :: String -> (OC.QVFDatum -> IO ()) -> IO ()
    actOnData datumJSON datumToIO = do
      -- {{{
      datumVal <- LBS.readFile datumJSON
      fromDatumValue datumVal datumToIO
      -- }}}

    infArgHelper :: (    String
                      -> String
                      -> a
                      -> Either String (a, (String, String, String))
                    )
                 -> Either String (a, (String, String, String))
                 -> [String]
                 -> Either String (a, (String, String, String))
    infArgHelper doubleArgFn initEith infArgs =
      -- {{{
      case (initEith, infArgs) of
        (Right (x, _), [curr, upd, rdmr] ) ->
          Right (x, (curr, upd, rdmr))
        (l@(Left _)  , _                 ) ->
          l
        (Right (x, _), el0 : el1 : rest  ) ->
          infArgHelper doubleArgFn (doubleArgFn el0 el1 x) rest
        (_           , _                 ) ->
          Left "Bad args."
      -- }}}

    mapKeyForKeyHolder :: String
    mapKeyForKeyHolder = "keyHolder"

    getDistributionMap :: OC.QVFDatum -> Map.Map String Integer
    getDistributionMap currDatum =
      -- {{{
      let
        projs        = OC.qvfProjects currDatum
        projectCount = fromIntegral $ length projs
        initialPool  = OC.qvfPool currDatum - OC.minLovelace * projectCount
        prizeMappings :: [(Ledger.PubKeyHash, Integer)]
        (extraFromRounding, prizeMappings) =
          OC.foldProjects initialPool projs
        foldFn (projPKH, prize) (keyHolderFees, soFar) =
          -- {{{
          let
            (finalPrize, forKH') = OC.getFinalPrizeAndKeyHoldersFee prize
            distMap = Map.insert projPKH finalPrize soFar
          in
          (forKH' + keyHolderFees, distMap)
          -- }}}
        (forKH, finalPrizeMap) =
          foldr foldFn (extraFromRounding, Map.empty) prizeMappings
      in
        Map.insert mapKeyForKeyHolder forKH
      $ Map.mapKeys
          (builtinByteStringToString . Ledger.getPubKeyHash)
          finalPrizeMap
      -- }}}

    donationHelper :: String
                   -> [String]
                   -> Either String ([OC.DonateParams], (String, String, String))
    donationHelper dDonor restOfArgs =
      -- {{{
      infArgHelper 
        ( \dProj dAmnt soFar ->
            -- {{{
            case readMaybe dAmnt of
              Nothing   ->
                -- {{{
                Left $ "Bad donation amount: " ++ dAmnt
                -- }}}
              Just amnt ->
                -- {{{
                Right
                  ( OC.DonateParams
                      { OC.dpDonor   = fromString dDonor
                      , OC.dpProject = fromString dProj
                      , OC.dpAmount  = amnt
                      }
                      : soFar
                  , ("", "", "")
                  )
                -- }}}
            -- }}}
        )
        (Right ([], ("", "", "")))
        restOfArgs
      -- }}}
    -- }}}
  in do
  allArgs <- getArgs
  case allArgs of
    []                                 -> printHelp
    "generate" : genStr : "-h"     : _ -> printGenerateHelp genStr
    "generate" : genStr : "--help" : _ -> printGenerateHelp genStr
    "generate" : genStr : "man"    : _ -> printGenerateHelp genStr
    actionStr  : "-h"     : _          -> printActionHelp actionStr
    actionStr  : "--help" : _          -> printActionHelp actionStr
    actionStr  : "man"    : _          -> printActionHelp actionStr
    "-h"       : _                     -> printHelp
    "--help"   : _                     -> printHelp
    "man"      : _                     -> printHelp
    "generate" : "scripts" : pkhStr : txRefStr : tn : deadlineStr : mOF : vOF : rOF : dOF : _ -> do
      -- {{{
      case (readTxOutRef txRefStr, Ledger.POSIXTime <$> readMaybe deadlineStr) of
        (Nothing   , _      ) ->
          -- {{{
          putStrLn "FAILED to parse the given UTxO."
          -- }}}
        (_         , Nothing) ->
          -- {{{
          putStrLn "FAILED to parse the deadline."
          -- }}}
        (Just txRef, Just dl) ->
          -- {{{
          let
            tokenName = fromString tn
          in do
          mintRes <- writeMintingPolicy mOF $ Token.qvfPolicy txRef tokenName
          case mintRes of
            Left _  ->
              -- {{{
              putStrLn "FAILED to write minting script file."
              -- }}}
            Right _ -> do
              -- {{{
              let tokenSymbol = Token.qvfSymbol txRef tokenName
                  qvfParams   =
                    OC.QVFParams
                      { OC.qvfKeyHolder  = fromString pkhStr
                      , OC.qvfSymbol     = tokenSymbol
                      , OC.qvfTokenName  = tokenName
                      }
              valRes <- writeValidator vOF $ OC.qvfValidator qvfParams
              case valRes of
                Left _  ->
                  -- {{{
                  putStrLn "FAILED to write Plutus script file."
                  -- }}}
                Right _ -> do
                  -- {{{
                  andPrintSuccess mOF $ return ()
                  andPrintSuccess vOF $ return ()
                  andPrintSuccess rOF $ writeJSON rOF ()
                  andPrintSuccess dOF $ writeJSON dOF (OC.initialDatum dl)
                  -- }}}
              -- }}}
          -- }}}
      -- }}}
    "pretty-datum" : datumJSONStr : _                                   ->
      -- {{{
      fromDatumValue (fromString datumJSONStr) print
      -- }}}
    "data-to-cbor" : dataJSONStr : _                                    ->
      -- {{{
      fromConcreteDataValue
        (fromString dataJSONStr)
        ( \parseError ->
            putStrLn $ "FAILED to parse data JSON: " ++ parseError
        )
        ( putStrLn
          . filter (\c -> c /= '"' && c /= '\\')
          . show
          . encode
        )
        -- }}}
    "string-to-hex" : tn : outFile : _                                  ->
      -- {{{
      andPrintSuccess outFile
        $ LBS.writeFile outFile
        $ fromString
        $ unsafeTokenNameToHex
        $ fromString tn
      -- }}}
    "add-project" : pPKH : pLabel : pReqStr : datumJSON : dOF : rOF : _ ->
      -- {{{
      actOnData datumJSON $ \currDatum ->
        case readMaybe pReqStr of
          Nothing ->
            -- {{{
            putStrLn "FAILED to parse the requested fund."
            -- }}}
          Just pReq ->
            -- {{{
            let
              action = OC.AddProject $ OC.AddProjectParams
                { OC.appPubKeyHash = fromString pPKH
                , OC.appLabel      = fromString pLabel
                , OC.appRequested  = pReq
                }
            in
            fromAction True action currDatum (Just dOF) rOF
            -- }}}
      -- }}}
    "donate" : dDonor : restOfArgs                                      ->
      -- {{{
      case donationHelper dDonor restOfArgs of
        Right (dPs, (datumJSON, dOF, rOF)) ->
          -- {{{
          actOnData datumJSON $ \currDatum ->
            fromAction True (OC.Donate dPs) currDatum (Just dOF) rOF
          -- }}}
        Left err                           ->
          -- {{{
          putStrLn $ "FAILED: " ++ err
          -- }}}
      -- }}}
    "contribute" : amountStr : datumJSON : dOF : rOF : _                ->
      -- {{{
      actOnData datumJSON $ \currDatum ->
        case readMaybe amountStr of
          Nothing ->
            -- {{{
            putStrLn "FAILED to parse the contribution amount."
            -- }}}
          Just amount ->
            -- {{{
            fromAction True (OC.Contribute amount) currDatum (Just dOF) rOF
            -- }}}
      -- }}}
    "set-deadline" : deadlineStr : datumJSON : dOF : rOF : _            ->
      -- {{{
      actOnData datumJSON $ \currDatum ->
        case Ledger.POSIXTime <$> readMaybe deadlineStr of
          Nothing ->
            -- {{{
            putStrLn "FAILED to parse the new deadline."
            -- }}}
          Just deadline ->
            -- {{{
            fromAction True (OC.SetDeadline deadline) currDatum (Just dOF) rOF
            -- }}}
      -- }}}
    "distribute" : keyHolderAddr : restOfArgs                           ->
      -- {{{
      let
        eith =
          -- {{{
          infArgHelper 
            ( \projPKH projAddr soFar ->
                -- {{{
                Right
                  ( Map.insert (fromString projPKH) projAddr soFar
                  , ("", "", "")
                  )
                -- }}}
            )
            (Right (Map.empty, ("", "", "")))
            restOfArgs
          -- }}}
      in
      case eith of
        Right (kvs, (datumJSON, dOF, rOF)) ->
          -- {{{
          actOnData datumJSON $ \currDatum ->
            let
              makeTxOutArg addr amt =
                -- {{{
                "--tx-out " ++ addr ++ "+" ++ show amt
                -- }}}
              foldFn :: String -> Integer -> Maybe String -> Maybe String
              foldFn recepientStr prize mCliArgs = do
                -- {{{
                cliArgs  <- mCliArgs
                projAddr <- if recepientStr == mapKeyForKeyHolder then
                              return keyHolderAddr
                            else
                              Map.lookup (fromString recepientStr) kvs
                return $ makeTxOutArg projAddr prize ++ " " ++ cliArgs
                -- }}}
              mArgs                 =
                -- {{{
                Map.foldrWithKey
                  foldFn
                  (Just "")
                  (getDistributionMap currDatum)
                -- }}}
            in
            case mArgs of
              Nothing   ->
                -- {{{
                putStrLn
                  "FAILED: Bad map from public key hashes to addresses."
                -- }}}
              Just args -> do
                -- {{{
                fromAction False OC.Distribute currDatum (Just dOF) rOF
                putStrLn args
                -- }}}
          -- }}}
        Left err                           ->
          -- {{{
          putStrLn $ "FAILED: " ++ err
          -- }}}
      -- }}}
    "emulate-distribution" : datumJSONStr : _                           ->
      -- {{{
      fromDatumValue
        (fromString datumJSONStr)
        (LBS8.putStrLn . encode . getDistributionMap)
      -- }}}
    "donation-impact" : dDonor : initRestOfArgs                         ->
      -- {{{
      let
        -- Expected arguments are pairs of targetPubKeyHashes and amounts,
        -- plus the current datum's JSON file at the very end. Since the
        -- `infArgHelper` abstraction expects exactly 3 files at the end of
        -- the list of arguments, these two "ignore" strings are injected at
        -- the end to prevent further complication of the abstraction.
        restOfArgs = initRestOfArgs ++ ["ignore", "ignore"]
      in
      case donationHelper dDonor restOfArgs of
        Right (dPs, (datumJSONStr, _, _)) ->
          -- {{{
          fromDatumValue (fromString datumJSONStr) $ \currDatum ->
            case OC.updateDatum (OC.Donate dPs) currDatum of
              Left err       ->
                -- {{{
                putStrLn $
                  "FAILED: Couldn't perform donation. Cause: " ++ show err
                -- }}}
              Right newDatum ->
                -- {{{
                let
                  currDist = getDistributionMap currDatum
                  newDist  = getDistributionMap newDatum
                  diffDist = Map.unionWith (-) newDist currDist
                in
                LBS8.putStrLn $ encode diffDist
                -- }}}
          -- }}}
        Left err                       ->
          -- {{{
          putStrLn $ "FAILED: " ++ err
          -- }}}
      -- }}}
    "get-deadline-slot" : currSlotStr : datumJSON : _                   -> do
      -- {{{
      case readMaybe currSlotStr of
        Just currSlot -> do
          -- {{{
          actOnData datumJSON $ \currDatum -> do
            let slotLength                = 1000
                Ledger.POSIXTime deadline = OC.qvfDeadline currDatum
            currPOSIX <- round . (* 1000) <$> getPOSIXTime -- milliseconds
            let diff = deadline - currPOSIX
            print $ diff `div` slotLength + currSlot
          -- }}}
        _ ->
          -- {{{
          putStrLn "FAILED: Couldn't parse current slot number.."
          -- }}}
      -- }}}
    _                                                                   ->
      putStrLn "FAILED: Invalid arguments for QVF-CLI."
-- }}}
