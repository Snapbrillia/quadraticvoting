{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE LambdaCase        #-}


module Main (main) where


import           Cardano.Api
import           Cardano.Api.Shelley        (PlutusScript (..))
import           Codec.Serialise            (Serialise, serialise)
import qualified Data.Aeson                 as A
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Short      as SBS
import qualified Data.List                  as List
import           Data.Maybe                 (fromJust)
import           Data.String                (fromString)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Plutus.V1.Ledger.Api       (fromBuiltin, BuiltinByteString)
import           Plutus.V1.Ledger.Value     (TokenName (..))
import qualified Plutus.V2.Ledger.Api       as Ledger
import           PlutusTx                   (Data (..))
import qualified PlutusTx
import           System.Environment         (getArgs)
import           Text.Read                  (readMaybe)

import           Datum

import qualified QVF                        as OC
import qualified Minter.Donation            as Don
import qualified Minter.Governance          as Gov
import qualified Minter.Registration        as Reg

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
    writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing
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


initialDatum :: QVFDatum
initialDatum = RegisteredProjectsCount 0
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
        , "<deadline-posix-milliseconds>"
        , "<governance-policy.plutus>"
        , "<registration-policy.plutus>"
        , "<donation-policy.plutus>"
        , "<output-validation.plutus>"
        , "<unit.redeemer>"
        , "<output-initial.datum>"
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
         "\nQVF off-chain assistive CLI application.\n\n"

      ++ "You can separately print the argument guide for each action\n"
      ++ "with (-h|--help|man) following the desired action. Available\n"
      ++ "options are:\n\n"

      ++ "Utility:\n"
      ++ "\tqvf-cli generate scripts     --help\n"
      ++ "\tqvf-cli pretty-datum         --help\n"
      ++ "\tqvf-cli data-to-cbor         --help\n"
      ++ "\tqvf-cli string-to-hex        --help\n"
      ++ "\tqvf-cli get-deadline-slot    --help\n\n"

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
        "pretty-datum"         ->
          putStrLn prettyDatumHelp
        "data-to-cbor"         ->
          putStrLn dataToCBORHelp
        "string-to-hex"        ->
          putStrLn toHexHelp
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
                         -> (QVFDatum -> IO a)
                         -> IO a
    fromDatumValueHelper datVal parseFailureAction badDataAction dataToIO =
      -- {{{
      fromConcreteDataValue datVal parseFailureAction $ \dataData ->
        maybe badDataAction dataToIO $ PlutusTx.fromData dataData
      -- }}}

    fromDatumValue :: LBS.ByteString
                   -> (QVFDatum -> IO ())
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

    actOnData :: String -> (QVFDatum -> IO ()) -> IO ()
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
    "generate" : "scripts" : pkhStr : txRefStr : deadlineStr : gOF : regOF : donOF : vOF : rOF : dOF : _ -> do
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
        (Just txRef, Just dl) -> do
          -- {{{
          govRes <- writeMintingPolicy gOF $ Gov.qvfPolicy txRef dl
          let qvfSymbol = Gov.qvfSymbol txRef dl
              regSymbol = Reg.registrationSymbol qvfSymbol
              donSymbol = Don.donationSymbol regSymbol
          regRes <- writeMintingPolicy regOF $ Reg.registrationPolicy qvfSymbol
          donRes <- writeMintingPolicy donOF $ Don.donationPolicy regSymbol
          case (govRes, regRes, donRes) of
            (Right _, Right _, Right _) -> do
              -- {{{
              let qvfParams =
                    OC.QVFParams
                      { OC.qvfKeyHolder      = fromString pkhStr
                      , OC.qvfSymbol         = qvfSymbol
                      , OC.qvfProjectSymbol  = regSymbol
                      , OC.qvfDonationSymbol = donSymbol
                      }
              valRes <- writeValidator vOF $ OC.qvfValidator qvfParams
              case valRes of
                Right _ -> do
                  -- {{{
                  andPrintSuccess gOF $ return ()
                  andPrintSuccess vOF $ return ()
                  andPrintSuccess rOF $ writeJSON rOF ()
                  andPrintSuccess dOF $ writeJSON dOF initialDatum
                  -- }}}
                Left _  ->
                  -- {{{
                  putStrLn "FAILED to write Plutus script file."
                  -- }}}
              -- }}}
            _                  ->
              -- {{{
              putStrLn "FAILED to write minting script files."
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
          . filter (\c -> c /= '"' && c /= '\\') -- "
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
    "get-deadline-slot" : currSlotStr : datumJSON : _                   -> do
      -- {{{
      case readMaybe currSlotStr of
        Just currSlot -> do
          -- {{{
          actOnData datumJSON $ \case
            DeadlineDatum (Ledger.POSIXTime deadline) -> do
              -- {{{
              let slotLength = 1000
              currPOSIX <- round . (* 1000) <$> getPOSIXTime -- milliseconds
              let diff       = deadline - currPOSIX
              print $ diff `div` slotLength + currSlot
              -- }}}
            _                                         ->
              -- {{{
              putStrLn "FAILED: Improper datum."
              -- }}}
          -- }}}
        _ ->
          -- {{{
          putStrLn "FAILED: Couldn't parse current slot number."
          -- }}}
      -- }}}
    _                                                                   ->
      putStrLn "FAILED: Invalid arguments for QVF-CLI."
-- }}}
