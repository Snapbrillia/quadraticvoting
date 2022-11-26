{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE GADTs             #-}


module CLI.Utils where


-- IMPORTS
-- {{{
import           Cardano.Api
import           Cardano.Api.Shelley         ( Address(..)
                                             , PlutusScript(..) )
import           Cardano.Crypto.Hash.Class   ( hashToBytes )
import           Cardano.Ledger.BaseTypes    ( certIxToInt
                                             , txIxToInt )
import           Cardano.Ledger.Credential   as CLedger
import           Cardano.Ledger.Crypto       ( StandardCrypto )
import           Cardano.Ledger.Hashes       ( ScriptHash(..) )
import           Cardano.Ledger.Keys         ( KeyHash(..) )
import           Codec.Serialise             ( Serialise
                                             , serialise )
import qualified Control.Monad.Fail          as M
import qualified Data.Aeson                  as A
import           Data.Aeson                  ( encode
                                             , FromJSON(..) )
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Short       as SBS
import qualified Data.Char                   as Char
import qualified Data.List                   as List
import           Data.Maybe                  ( fromJust )
import           Data.String                 ( fromString )
import           Data.Text                   ( Text )
import qualified Data.Text                   as Text
import           Data.Time.Clock.POSIX       ( getPOSIXTime )
import           GHC.Generics                ( Generic )
import           Plutus.V1.Ledger.Api        ( fromBuiltin
                                             , toBuiltin
                                             , BuiltinByteString )
import qualified Plutus.V1.Ledger.Credential as Plutus
import qualified Plutus.V1.Ledger.Crypto     as Plutus
import           Plutus.V1.Ledger.Value      ( CurrencySymbol(..)
                                             , TokenName(..) )
import           Plutus.V2.Ledger.Api        ( TxOutRef(..)
                                             , ToData(..) )
import qualified Plutus.V2.Ledger.Api        as Ledger
import           PlutusTx                    ( Data (..) )
import qualified PlutusTx
import qualified PlutusTx.AssocMap           as Map
import           PlutusTx.Prelude            ( lengthOfByteString )
import           Prelude

import           Data.Datum
import           Data.Redeemer
import           Utils
-- }}}


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


credentialLedgerToPlutus :: CLedger.Credential a StandardCrypto
                         -> Plutus.Credential
credentialLedgerToPlutus (ScriptHashObj (ScriptHash h)) =
  Plutus.ScriptCredential $ Ledger.ValidatorHash $ toBuiltin $ hashToBytes h
credentialLedgerToPlutus (KeyHashObj (KeyHash h))       =
  Plutus.PubKeyCredential $ Plutus.PubKeyHash $ toBuiltin $ hashToBytes h


stakeReferenceLedgerToPlutus :: CLedger.StakeReference StandardCrypto
                             -> Maybe Plutus.StakingCredential
stakeReferenceLedgerToPlutus (CLedger.StakeRefBase x)                   =
  Just $ Ledger.StakingHash $ credentialLedgerToPlutus x
stakeReferenceLedgerToPlutus (CLedger.StakeRefPtr (CLedger.Ptr (SlotNo x) y z)) =
  Just $
    Ledger.StakingPtr
      (fromIntegral x)
      (fromIntegral $ txIxToInt y)
      (fromIntegral $ certIxToInt z)
stakeReferenceLedgerToPlutus CLedger.StakeRefNull                       =
  Nothing


tryReadAddress :: Text -> Maybe Ledger.Address
tryReadAddress x =
  case deserialiseAddress AsAddressAny x of
    Nothing                                      ->
      Nothing
    Just (AddressByron _)                        ->
      Nothing
    Just (AddressShelley (ShelleyAddress _ p s)) ->
      Just Ledger.Address
        { Ledger.addressCredential        = credentialLedgerToPlutus p
        , Ledger.addressStakingCredential = stakeReferenceLedgerToPlutus s
        }


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


parseJSONValue :: LBS.ByteString -> Either String Data
parseJSONValue bs =
  -- {{{
  case A.decode bs of
    Just decoded ->
      case scriptDataFromJson ScriptDataJsonDetailedSchema decoded of
        Right scriptData ->
          Right (scriptDataToData scriptData)
        Left err ->
          Left $ show err
    Nothing ->
      Left "Invalid JSON."
  -- }}}


dataToCBOR :: Data -> String
dataToCBOR =
  -- {{{
    filter (\c -> c /= '"' && c /= '\\') -- "
  . show
  . encode
  -- }}}

typedToCBOR :: ToData a => a -> String
typedToCBOR =
  -- {{{
    dataToCBOR
  . PlutusTx.toData
  . toBuiltinData
  -- }}}


readDatum :: String -> Maybe QVFDatum
readDatum datStr =
  -- {{{
  case parseJSONValue (fromString datStr) of
    Right d ->
      PlutusTx.fromData d
    Left _  ->
      Nothing
  -- }}}


readQVFDatum :: String -> Maybe QVFDatum
readQVFDatum str =
  -- {{{
  case str of
    "DeadlineDatum"                 ->
      -- {{{
      Just $ DeadlineDatum $ Ledger.POSIXTime 0
      -- }}}
    "RegisteredProjectsCount"       ->
      -- {{{
      Just $ RegisteredProjectsCount 0
      -- }}}
    "PrizeWeightAccumulation"       ->
      -- {{{
      Just $ PrizeWeightAccumulation 0 Map.empty
      -- }}}
    "ProjectEliminationProgress"    ->
      -- {{{
      Just $ ProjectEliminationProgress 0 Map.empty
      -- }}}
    "DistributionProgress"          ->
      -- {{{
      Just $ DistributionProgress 0 0 0
      -- }}}
    "ProjectInfo"                   ->
      -- {{{
      Just $ ProjectInfo $ ProjectDetails
        (fromString "00000000000000000000000000000000000000000000000000000000")
        ""
        0
      -- }}}
    "ReceivedDonationsCount"        ->
      -- {{{
      Just $ ReceivedDonationsCount 0
      -- }}}
    "DonationFoldingProgress"       ->
      -- {{{
      Just $ DonationFoldingProgress 0 0
      -- }}}
    "ConsolidationProgress"         ->
      -- {{{
      Just $ ConsolidationProgress 0 0
      -- }}}
    "PrizeWeight"                   ->
      -- {{{
      Just $ PrizeWeight 0 False
      -- }}}
    "Escrow"                        ->
      -- {{{
      Just $ Escrow Map.empty
      -- }}}
    "Donation"                      ->
      -- {{{
      Just $ Donation 
        (fromString "00000000000000000000000000000000000000000000000000000000")
      -- }}}
    "Donations"                     ->
      -- {{{
      Just $ Donations Map.empty
      -- }}}
    _                               ->
      -- {{{
      Nothing
      -- }}}
  -- }}}


unsafeParseJSON :: A.FromJSON a => FilePath -> IO a
unsafeParseJSON file = do
  -- {{{
  fileContent <- LBS.readFile file
  let Just decoded = A.decode fileContent
  return decoded
  -- }}}


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


hexStringToByteString :: String -> Maybe LBS.ByteString
hexStringToByteString str =
  -- {{{
  let
    helper =
      -- {{{
      \case
        '0' -> Just 0
        '1' -> Just 1
        '2' -> Just 2
        '3' -> Just 3
        '4' -> Just 4
        '5' -> Just 5
        '6' -> Just 6
        '7' -> Just 7
        '8' -> Just 8
        '9' -> Just 9
        'a' -> Just 10
        'b' -> Just 11
        'c' -> Just 12
        'd' -> Just 13
        'e' -> Just 14
        'f' -> Just 15
        _   -> Nothing
      -- }}}
    go []                (Just _, _    ) = Nothing
    go []                (_     , soFar) = Just $ LBS.pack soFar
    go (currChar : rest) (mPrev , soFar) =
      case mPrev of
        Just prev ->
          -- {{{
          let
            mVal = do
              v1 <- helper currChar
              v0 <- helper prev
              return $ 16 * v0 + v1
          in
          case mVal of
            Just w ->
              go rest (Nothing, soFar ++ [w])
            Nothing ->
              Nothing
          -- }}}
        Nothing   ->
          -- {{{
          go rest (Just currChar, soFar)
          -- }}}
  in
  go str (Nothing, [])
  -- }}}


hexStringToBuiltinByteString :: String -> Maybe BuiltinByteString
hexStringToBuiltinByteString =
  -- {{{
  fmap (toBuiltin . LBS.toStrict) . hexStringToByteString
  -- }}}


cborStringToData :: String -> Maybe Data
cborStringToData cborStr =
  -- {{{
  let
    mScriptData :: Maybe ScriptData
    mScriptData = do
      -- {{{
      bs  <- hexStringToByteString cborStr
      let bs8 = BS8.pack $ Char.chr . fromIntegral <$> LBS.unpack bs
      case deserialiseFromCBOR AsScriptData bs8 of
        Right sd ->
          Just sd
        Left _   ->
          Nothing
      -- }}}
  in do
  sd <- mScriptData
  -- return $ encode $ scriptDataToJson ScriptDataJsonDetailedSchema sd
  return $ scriptDataToData sd
  -- }}}


readTxOutRef :: String -> Maybe TxOutRef
readTxOutRef s =
  -- {{{
  case span (/= '#') s of
    (x, _ : y) ->
      -- {{{
      Just $ TxOutRef
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
  -- {{{
    BS8.unpack
  . serialiseToRawBytesHex
  . fromJust
  . deserialiseFromRawBytes AsAssetName
  . fromBuiltin
  -- }}}


-- | From PPP's 6th lecture.
unsafeTokenNameToHex :: TokenName -> String
unsafeTokenNameToHex =
  -- {{{
  builtinByteStringToString . unTokenName
  -- }}}


deadlineDatum :: Ledger.POSIXTime -> QVFDatum
deadlineDatum = DeadlineDatum

initialGovDatum :: QVFDatum
initialGovDatum = RegisteredProjectsCount 0


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
toHexHelp          :: String
toHexHelp          =
  -- {{{
  makeHelpText
    "Export the hex serialization of a token name:"
    "string-to-hex"
    "<token-name>"
    ["<output.hex>"]
  -- }}}
scriptHelp         :: String
scriptHelp         =
  -- {{{
  makeHelpText
    (    "Generate the compiled Plutus validation and minting scripts.\n\n"

      ++ "\tThe JSON for file names should have these fields:\n\n"
      ++ "\t\t, ocfnGovernanceMinter     :: String\n"
      ++ "\t\t, ocfnRegistrationMinter   :: String\n"
      ++ "\t\t, ocfnDonationMinter       :: String\n"
      ++ "\t\t, ocfnQVFMainValidator     :: String\n"
      ++ "\t\t, ocfnDeadlineSlot         :: String\n"
      ++ "\t\t, ocfnDeadlineDatum        :: String\n"
      ++ "\t\t, ocfnInitialGovDatum      :: String\n"
      ++ "\t\t, ocfnCurrentDatum         :: String\n"
      ++ "\t\t, ocfnUpdatedDatum         :: String\n"
      ++ "\t\t, ocfnNewDatum             :: String\n"
      ++ "\t\t, ocfnQVFRedeemer          :: String\n"
      ++ "\t\t, ocfnMinterRedeemer       :: String\n"
      ++ "\t\t}"
    )
    "generate scripts"
    "<key-holder-pub-key-hash>"
    [ "<txID>#<output-index>"
    , "<current-slot-number>"
    , "<deadline-posix-milliseconds>"
    , "{file-names-json}"
    ]
  -- }}}
registrationHelp   :: String
registrationHelp   =
  -- {{{
  makeHelpText
    (    "Read the current datum from disk, and write the corresponding files:\n"
      ++ "\t\t- Updated governance datum,\n"
      ++ "\t\t- New initial project datum,\n"
      ++ "\t\t- Static datum for project's info,\n"
      ++ "\t\t- Redeemer for the QVF validator,\n"
      ++ "\t\t- Redeemer for the registration policy."
    )
    "register-project"
    "<project-owner-pub-key-hash>"
    [ "<project-name>"
    , "<project-requested-fund>"
    , "{file-names-json}"
    ]
  -- }}}
donationHelp       :: String
donationHelp       =
  -- {{{
  makeHelpText
    (    "Read the current datum from disk, and write the corresponding files:\n"
      ++ "\t\t- Updated project datum,\n"
      ++ "\t\t- New donation datum,\n"
      ++ "\t\t- Redeemer for the QVF validator,\n"
      ++ "\t\t- Redeemer for the donation policy."
    )
    "donate-to-project"
    "<donors-pub-key-hash>"
    [ "<target-project-id>"
    , "<donation-amount>"
    , "{file-names-json}"
    ]
  -- }}}
foldingHelp        :: String
foldingHelp        =
  -- {{{
  makeHelpText
    (    "Read the current project datum from disk, and write the\n"
      ++ "\tupdated project datum to disk. Also prints a JSON\n"
      ++ "\twith two fields of \"lovelace\" and \"mint\" to help\n"
      ++ "\tthe bash script construct the `tx-out` argument.\n"
    )
    "fold-donations"
    "<donation(s)-lovelace-count-0>"
    [ "<donation-count-------------0>"
    , "{donation-datum-json--------0}"
    , "<donation(s)-lovelace-count-1>"
    , "<donation-count-------------1>"
    , "{donation-datum-json--------1}"
    , "<...etc...>"
    , "{file-names-json}"
    ]
  -- }}}
consolidationHelp  :: String
consolidationHelp  =
  -- {{{
  makeHelpText
    (    "Read the current project datum from disk, and write the\n"
      ++ "\tupdated project datum to disk. Also prints a JSON\n"
      ++ "\twith two fields of \"lovelace\" and \"mint\" to help\n"
      ++ "\tthe bash script construct the `tx-out` argument.\n"
      ++ "\tThe value in the \"lovelace\" field is the amound that\n"
      ++ "\tshould be added to the input main UTxO.\n"
    )
    "consolidate-donations"
    "<donation(s)-lovelace-count-0>"
    [ "<donation-count-------------0>"
    , "{folded-donation-datum-json-0}"
    , "<donation(s)-lovelace-count-1>"
    , "<donation-count-------------1>"
    , "{folded-donation-datum-json-1}"
    , "<...etc...>"
    , "{file-names-json}"
    ]
  -- }}}
traversalHelp      :: String
traversalHelp      =
  -- {{{
  makeHelpText
    (    "Takes information of two input fully folded donation UTxOs,\n"
      ++ "\tcompares their donations, and if duplicates where found,\n"
      ++ "\tit'll return a JSON object with \"lovelace0\" and \"lovelace1\"\n"
      ++ "\tfields (as resolving the duplication requires exchange of\n"
      ++ "\tsome Lovelaces). Reallocation of donation assets doesn't\n"
      ++ "\tseem necessary at this point.\n"
      ++ "\tIf there are no overlaps, the \"Nothing\" string is returned.\n"
    )
    "traverse-donations"
    "<donations-lovelace-count-0>"
    [ "{donations-datum-json-----0}"
    , "<donations-lovelace-count-1>"
    , "{donations-datum-json-----1}"
    , "{file-names-json}"
    ]
  -- }}}
accumulationHelp   :: String
accumulationHelp   =
  -- {{{
  makeHelpText
    (    "Read the current governance datum from disk, and write the\n"
      ++ "\tupdated datum to disk. Also writes a dedicated file (labeled\n"
      ++ "\tthe same as the token name of the project with a `.datum`\n"
      ++ "\textension) where the updated datum of that project is stored.\n"
      ++ "\tThe input Lovelace count, minus all the registration fees\n"
      ++ "\t(as they are meant to be included in the inputs) is what gets\n"
      ++ "\tprinted.\n"
    )
    "accumulate-donations"
    "<project-id-------------0>"
    [ "<project-lovelace-count-0>"
    , "{project-datum-json-----0}"
    , "<project-id-------------1>"
    , "<project-lovelace-count-1>"
    , "{project-datum-json-----1}"
    , "<...etc...>"
    , "{file-names-json}"
    ]
  -- }}}
khFeeHelp          :: String
khFeeHelp          =
  -- {{{
  makeHelpText
    (    "The only required argument is the JSON of the filenames. The\n"
      ++ "\tprinted value is the Lovelace count that should be paid to\n"
      ++ "\tthe key holder."
    )
    "collect-key-holder-fee"
    "{file-names-json}"
    []
  -- }}}
distributionHelp   :: String
distributionHelp   =
  -- {{{
  makeHelpText
    (    "With the project's info datum and prize weight provided,\n"
      ++ "\tthis endpoint reads the current governance datum from disk,\n"
      ++ "\twrites the updated datum, and also writes the escrow datum\n"
      ++ "\tinside the \"new datum\" file on disk.\n"
      ++ "\tThe printed output is a JSON with two fields, \"owner\",\n"
      ++ "\tholding the number of Lovelaces that should be sent to the\n"
      ++ "\tproject owner, and \"escrow\" that holds the Lovelace count\n"
      ++ "\tthat must be stored inside the produced escrow datum."
    )
    "distribute-prize"
    "{project-info-datum-json}"
    [ "{project-weight-datum-json}"
    , "{file-names-json}"
    ]
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
checkDatumHelp     :: String
checkDatumHelp     =
  -- {{{
  makeHelpText
    (    "Check whether a given data JSON decodes to a specific `QVFDatum`\n"
      ++ "\t(returns either \"True\" or \"False\"). Supported keywords are:\n"
      ++ "\t\tDeadlineDatum\n"
      ++ "\t\tProjectInfo"
      ++ "\t\tDonationAccumulationConcluded"
    )
    "datum-is"
    "<predicate-keyword>"
    ["{current-datum-json-value}"]
  -- }}}
getConstrHelp      :: String
getConstrHelp      =
  -- {{{
  makeHelpText
    (    "Given a constructor from `QVFDatum`, this endpoints returns the\n"
      ++ "\tconstructor index of the `Data` equivalent.\n"
    )
    "get-constr-index"
    "<datum-constructor>"
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
cborToDataHelp     :: String
cborToDataHelp     =
  -- {{{
  makeHelpText
    "Attempt decoding a CBOR to a JSON formatted `Data` value:"
    "cbor-to-data"
    "<cbor-hex-string>"
    []
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
helpText :: String
helpText =
  -- {{{
     "\nQVF off-chain assistive CLI application.\n\n"

  ++ "You can separately print the argument guide for each action\n"
  ++ "with (-h|--help|man) following the desired action. Available\n"
  ++ "options are:\n\n"

  ++ "Utility:\n"
  ++ "\tqvf-cli generate scripts       --help\n"
  ++ "\tqvf-cli register-project       --help\n"
  ++ "\tqvf-cli donate-to-project      --help\n"
  ++ "\tqvf-cli fold-donations         --help\n"
  ++ "\tqvf-cli consolidate-donations  --help\n"
  ++ "\tqvf-cli traverse-donations     --help\n"
  ++ "\tqvf-cli accumulate-donations   --help\n"
  ++ "\tqvf-cli collect-key-holder-fee --help\n"
  ++ "\tqvf-cli distribute-prize       --help\n"
  ++ "\tqvf-cli pretty-datum           --help\n"
  ++ "\tqvf-cli datum-is               --help\n"
  ++ "\tqvf-cli data-to-cbor           --help\n"
  ++ "\tqvf-cli cbor-to-data           --help\n"
  ++ "\tqvf-cli string-to-hex          --help\n"
  ++ "\tqvf-cli get-deadline-slot      --help\n\n"

  ++ "Or simply use (-h|--help|man) to print this help text.\n\n"
  -- }}}


printHelp :: IO ()
printHelp = putStrLn helpText


andPrintSuccess :: FilePath -> IO () -> IO ()
andPrintSuccess outFile ioAction = do
  -- {{{
  ioAction
  putStrLn $ outFile ++ " generated SUCCESSFULLY."
  -- }}}


andPrintSuccessWithSize :: Show a
                         => FilePath
                         -> (FilePath -> IO a)
                         -> IO ()
                         -> IO ()
andPrintSuccessWithSize outFile getByteCount ioAction = do
  -- {{{
  ioAction
  printable <- getByteCount outFile
  let byteCount = show printable
  putStrLn $
    outFile ++ " generated SUCCESSFULLY (" ++ byteCount ++ " bytes)."
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
    "register-project"       ->
      putStrLn registrationHelp
    "donate-to-project"      ->
      putStrLn donationHelp
    "fold-donations"         ->
      putStrLn foldingHelp
    "consolidate-donations"  ->
      putStrLn consolidationHelp
    "traverse-donations"     ->
      putStrLn traversalHelp
    "accumulate-donations"   ->
      putStrLn accumulationHelp
    "collect-key-holder-fee" ->
      putStrLn khFeeHelp
    "distribute-prize"       ->
      putStrLn distributionHelp
    "pretty-datum"           ->
      putStrLn prettyDatumHelp
    "datum-is"               ->
      putStrLn checkDatumHelp
    "data-to-cbor"           ->
      putStrLn dataToCBORHelp
    "cbor-to-data"           ->
      putStrLn cborToDataHelp
    "string-to-hex"          ->
      putStrLn toHexHelp
    "get-deadline-slot"      ->
      putStrLn deadlineToSlotHelp
    _                        ->
      printHelp
  -- }}}


predicateKeyWordToPredicate :: String -> Maybe (QVFDatum -> Bool)
predicateKeyWordToPredicate kw =
  -- {{{
  case kw of
    "ProjectInfo"                   ->
      Just $ \case
        ProjectInfo _ -> True
        _             -> False
    "DeadlineDatum"                 ->
      Just $ \case
        DeadlineDatum _ -> True
        _               -> False
    _               ->
      Nothing
  -- }}}


fromConcreteDataValue :: LBS.ByteString
                      -> (String -> IO a)
                      -> (Data -> IO a)
                      -> IO a
fromConcreteDataValue datVal parseFailureAction dataToIO = do
  -- {{{
  let eitherErrData = parseJSONValue datVal
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


-- | Implicitly, and silently, writes the redeemer value(s) to disk.
actOnCurrentDatum :: PlutusTx.ToData a 
                  => OffChainFileNames
                  -> QVFRedeemer
                  -> Maybe a
                  -> (QVFDatum -> IO ())
                  -> IO ()
actOnCurrentDatum ocfn qvfRedeemer mMinterRedeemer datumToIO = do
  -- {{{
  let datumJSON       = getFileName ocfn ocfnCurrentDatum
      qvfRedeemerFile = getFileName ocfn ocfnQVFRedeemer
  writeJSON qvfRedeemerFile qvfRedeemer
  case mMinterRedeemer of
    Nothing             ->
      -- {{{
      return ()
      -- }}}
    Just minterRedeemer ->
      -- {{{
      let
        minterRedeemerFile = getFileName ocfn ocfnMinterRedeemer
      in
      writeJSON minterRedeemerFile minterRedeemer
      -- }}}
  datumVal <- LBS.readFile datumJSON
  fromDatumValue datumVal datumToIO
  -- }}}


actOnData :: String -> (QVFDatum -> IO ()) -> IO ()
actOnData datumJSON datumToIO = do
  -- {{{
  datumVal <- LBS.readFile datumJSON
  fromDatumValue datumVal datumToIO
  -- }}}


infArgHelper :: (String -> a -> Either String a)
             -> Either String a
             -> [String]
             -> Either String (a, OffChainFileNames)
infArgHelper argFn initEith infArgs =
  -- {{{
  case (initEith, infArgs) of
    (Right x   , [ocfnStr] ) ->
      -- {{{
      case A.decode (fromString ocfnStr) of
        Just ocfn ->
          Right (x, ocfn)
        Nothing   ->
          Left "Could not parse file names JSON."
      -- }}}
    (Right x   , el0 : rest) ->
      -- {{{
      infArgHelper argFn (argFn el0 x) rest
      -- }}}
    (Left err  , _         ) ->
      -- {{{
      Left err
      -- }}}
    (_         , _         ) ->
      -- {{{
      Left "Bad arguments."
      -- }}}
  -- }}}


infArgHelper2 :: (String -> String -> a -> Either String a)
              -> Either String a
              -> [String]
              -> Either String (a, OffChainFileNames)
infArgHelper2 argFn initEith infArgs =
  -- {{{
  case (initEith, infArgs) of
    (Right x   , [ocfnStr]       ) ->
      -- {{{
      case A.decode (fromString ocfnStr) of
        Just ocfn ->
          Right (x, ocfn)
        Nothing   ->
          Left "Could not parse file names JSON."
      -- }}}
    (Right x   , el0 : el1 : rest) ->
      -- {{{
      infArgHelper2 argFn (argFn el0 el1 x) rest
      -- }}}
    (Left err  , _               ) ->
      -- {{{
      Left err
      -- }}}
    (_         , _               ) ->
      -- {{{
      Left "Bad arguments."
      -- }}}
  -- }}}


infArgHelper3 :: (    String
                   -> String
                   -> String
                   -> a
                   -> Either String a
                 )
              -> Either String a
              -> [String]
              -> Either String (a, OffChainFileNames)
infArgHelper3 triArgFn initEith infArgs =
  -- {{{
  case (initEith, infArgs) of
    (Right x   , [ocfnStr]               ) ->
      -- {{{
      case A.decode (fromString ocfnStr) of
        Just ocfn ->
          Right (x, ocfn)
        Nothing   ->
          Left "Could not parse file names JSON."
      -- }}}
    (Right x   , el0 : el1 : el2 : rest  ) ->
      -- {{{
      infArgHelper3 triArgFn (triArgFn el0 el1 el2 x) rest
      -- }}}
    (Left err  , _                       ) ->
      -- {{{
      Left err
      -- }}}
    (_         , _                       ) ->
      -- {{{
      Left "Bad arguments."
      -- }}}
  -- }}}


writeTokenNameHex :: FilePath -> TokenName -> IO ()
writeTokenNameHex outFile tn =
  -- {{{
    LBS.writeFile outFile
  $ fromString
  $ unsafeTokenNameToHex tn
  -- }}}


getDeadlineSlot :: Integer -> Ledger.POSIXTime -> IO Integer
getDeadlineSlot currSlot (Ledger.POSIXTime deadline) = do
  -- {{{
  let slotLength = 1000
  currPOSIX <- round . (* 1000) <$> getPOSIXTime -- milliseconds
  let diff = deadline - currPOSIX
  return $ diff `div` slotLength + currSlot
  -- }}}


decodeFromString :: FromJSON a => String -> Maybe a
decodeFromString = A.decode . fromString

-- EXTRA
-- {{{
data OutputPlutus = OutputPlutus
  { cborHex :: BuiltinByteString
  } deriving (Generic, A.ToJSON, A.FromJSON)
instance Show OutputPlutus where
  show OutputPlutus{..} =
    show $ lengthOfByteString cborHex


-- TODO: Create a module that only exposes the `OffChainFileNames` type
--       constructor, and the `getFileName` without exposing any of the
--       handles. Current solution involves use of type-level string literals.
--
--       Such a design would allow a more robust filename extraction (with
--       absolute paths). An outcome similar to the code snippet below
--       (utilizing the `TypeApplications` extension) would be desireable.
--
--       (Sample excerpt from the script generation endpoint of `main`.)
--
--       ```hs
--       let govOF     = getFileName ocfn @"ocfnGovernanceMinter"
--           regOF     = getFileName ocfn @"ocfnRegistrationMinter"
--           donOF     = getFileName ocfn @"ocfnDonationMinter"
--           qvfOF     = getFileName ocfn @"ocfnQVFMainValidator"
--           dlDatOF   = getFileName ocfn @"ocfnDeadlineDatum"
--           initDatOF = getFileName ocfn @"ocfnInitialGovDatum"
--           dlSlotOF  = getFileName ocfn @"ocfnDeadlineSlot"
--       ```
--
--       Another solution could be a custom `FromJSON` instance.
--
data OffChainFileNames = OffChainFileNames
  { ocfnPreDir               :: String
  , ocfnProjectsPreDir       :: String
  , ocfnGovernanceMinter     :: String
  , ocfnRegistrationMinter   :: String
  , ocfnDonationMinter       :: String
  , ocfnQVFMainValidator     :: String
  , ocfnDeadlineSlot         :: String
  , ocfnDeadlineDatum        :: String
  , ocfnInitialGovDatum      :: String
  , ocfnCurrentDatum         :: String
  , ocfnUpdatedDatum         :: String
  , ocfnNewDatum             :: String
  , ocfnQVFRedeemer          :: String
  , ocfnMinterRedeemer       :: String
  , ocfnProjectTokenName     :: String
  } deriving (Generic, A.ToJSON, A.FromJSON)


getFileName :: OffChainFileNames -> (OffChainFileNames -> String) -> FilePath
getFileName ocfn handle =
  ocfnPreDir ocfn ++ "/" ++ handle ocfn


getProjectsDatumFile :: OffChainFileNames -> TokenName -> FilePath
getProjectsDatumFile ocfn tn =
  ocfnPreDir ocfn ++ "/" ++ ocfnProjectsPreDir ocfn ++ "/" ++ unsafeTokenNameToHex tn ++ ".datum"


data ScriptGenerationArgumentsParseResults =
  ScriptGenerationArgumentsParseResults
    { sgUTxO      :: Maybe TxOutRef
    , sgSlot      :: Maybe Integer
    , sgDeadline  :: Maybe Ledger.POSIXTime
    , sgFileNames :: Maybe OffChainFileNames
    }


unwrapScriptGenerationArgs :: ScriptGenerationArgumentsParseResults
                           -> Either String ( TxOutRef
                                            , Integer
                                            , Ledger.POSIXTime
                                            , OffChainFileNames
                                            )
unwrapScriptGenerationArgs ScriptGenerationArgumentsParseResults{..} =
  -- {{{
  case (sgUTxO, sgSlot, sgDeadline, sgFileNames) of
    (Just utxo, Just slot, Just dl, Just fns) ->
      -- {{{
      Right (utxo, slot, dl, fns)
      -- }}}
    _                                         ->
      -- {{{
      let
        listPrefix     = "\n\t- "
        utxoError      =
          -- {{{
          case sgUTxO of
            Just _  -> ""
            Nothing -> listPrefix ++ "Invalid UTxO"
          -- }}}
        slotError      =
          -- {{{
          case sgSlot of
            Just _  -> ""
            Nothing -> listPrefix ++ "Invalid current slot"
          -- }}}
        deadlineError  =
          -- {{{
          case sgDeadline of
            Just _  -> ""
            Nothing -> listPrefix ++ "Invalid deadline POSIX"
          -- }}}
        fileNamesError =
          -- {{{
          case sgFileNames of
            Just _  -> ""
            Nothing -> listPrefix ++ "Invalid JSON for file names"
          -- }}}
      in
      Left $
           "FAILED: One or more bad arguments found:"
        ++ utxoError
        ++ slotError
        ++ deadlineError
        ++ fileNamesError
      -- }}}
  -- }}}


handleScriptGenerationArguments :: ScriptGenerationArgumentsParseResults
                                -> (TxOutRef -> Integer -> Ledger.POSIXTime -> OffChainFileNames -> IO ())
                                -> IO ()
handleScriptGenerationArguments results handler =
  -- {{{
  case unwrapScriptGenerationArgs results of
    Right (txRef, currSlot, dl, ocfn) ->
      handler txRef currSlot dl ocfn
    Left errMsg                       ->
      putStrLn errMsg
  -- }}}
-- }}}




