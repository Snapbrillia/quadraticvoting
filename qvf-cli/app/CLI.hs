{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE LambdaCase        #-}


module Main (main) where


import Debug.Trace (trace)

import           Cardano.Api
import           Cardano.Api.Shelley        ( PlutusScript(..) )
import           Codec.Serialise            ( Serialise
                                            , serialise )
import qualified Data.Aeson                 as A
import           Data.Aeson                 ( encode )
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Short      as SBS
import qualified Data.Char                  as Char
import qualified Data.List                  as List
import           Data.Maybe                 ( fromJust )
import           Data.String                ( fromString )
import           Data.Time.Clock.POSIX      ( getPOSIXTime )
import           GHC.Generics               ( Generic )
import           Plutus.V1.Ledger.Api       ( fromBuiltin
                                            , toBuiltin
                                            , BuiltinByteString )
import           Plutus.V1.Ledger.Value     ( TokenName(..) )
import qualified Plutus.V2.Ledger.Api       as Ledger
import           PlutusTx                   ( Data (..) )
import qualified PlutusTx
import           PlutusTx.Prelude           ( lengthOfByteString )
import           System.Environment         ( getArgs )
import           Text.Read                  ( readMaybe )

import           Data.Datum
import           Data.DonationInfo
import           Data.Redeemer
import           Data.RegistrationInfo

import qualified QVF                        as OC
import qualified Minter.Donation            as Don
import qualified Minter.Governance          as Gov
import qualified Minter.Registration        as Reg
import           Utils

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


unsafeParseJSON :: A.FromJSON a => FilePath -> IO a
unsafeParseJSON file = do
  -- {{{
  fileContent <- LBS.readFile file
  let Just decoded = A.decode fileContent
  return decoded
  -- }}}


-- unsafeParsePlutusJSON :: FilePath -> IO OutputPlutus
-- unsafeParsePlutusJSON = unsafeParseJSON


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


deadlineDatum :: Ledger.POSIXTime -> QVFDatum
deadlineDatum = DeadlineDatum

initialGovDatum :: QVFDatum
initialGovDatum = RegisteredProjectsCount 0
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
          ++ "\t\t{ ocfnDeadlineTokenNameHex :: String\n"
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
        "<txID>#<output-index>"
        [ "<project-owner-pub-key-hash>"
        , "<project-name>"
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
    cborToDataHelp     :: String
    cborToDataHelp     =
      -- {{{
      makeHelpText
        "Return the CBOR encoding of a JSON formatted `Data` value:"
        "cbor-to-data"
        "{arbitrary-data-json-value}"
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
    helpText          :: String
    helpText          =
      -- {{{
         "\nQVF off-chain assistive CLI application.\n\n"

      ++ "You can separately print the argument guide for each action\n"
      ++ "with (-h|--help|man) following the desired action. Available\n"
      ++ "options are:\n\n"

      ++ "Utility:\n"
      ++ "\tqvf-cli generate scripts  --help\n"
      ++ "\tqvf-cli register-project  --help\n"
      ++ "\tqvf-cli donate-to-project --help\n"
      ++ "\tqvf-cli pretty-datum      --help\n"
      ++ "\tqvf-cli data-to-cbor      --help\n"
      ++ "\tqvf-cli cbor-to-data      --help\n"
      ++ "\tqvf-cli string-to-hex     --help\n"
      ++ "\tqvf-cli get-deadline-slot --help\n\n"

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
        "register-project"     ->
          putStrLn registrationHelp
        "donate-to-project"    ->
          putStrLn donationHelp
        "pretty-datum"         ->
          putStrLn prettyDatumHelp
        "data-to-cbor"         ->
          putStrLn dataToCBORHelp
        "cbor-to-data"         ->
          putStrLn cborToDataHelp
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

    actOnCurrentDatum :: PlutusTx.ToData a 
                      => OffChainFileNames
                      -> QVFAction
                      -> Maybe a
                      -> (QVFDatum -> IO ())
                      -> IO ()
    actOnCurrentDatum ocfn qvfRedeemer mMinterRedeemer datumToIO = do
      -- {{{
      let datumJSON       = getFileName ocfn ocfnCurrentDatum
          qvfRedeemerFile = getFileName ocfn ocfnQVFRedeemer
      andPrintSuccess qvfRedeemerFile $ writeJSON qvfRedeemerFile qvfRedeemer
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
          andPrintSuccess minterRedeemerFile $
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
    "generate" : "scripts" : pkhStr : txRefStr : currSlotStr : deadlineStr : fileNamesJSON : _ ->
      -- {{{
      let
        results = ScriptGenerationArgumentsParseResults
          { sgUTxO      = readTxOutRef txRefStr
          , sgSlot      = readMaybe currSlotStr
          , sgDeadline  = Ledger.POSIXTime <$> readMaybe deadlineStr
          , sgFileNames = A.decode $ fromString fileNamesJSON
          }
      in
      handleScriptGenerationArguments results $
        \txRef currSlot dl ocfn -> do
          -- {{{
          let govOF     = getFileName ocfn ocfnGovernanceMinter
              regOF     = getFileName ocfn ocfnRegistrationMinter
              donOF     = getFileName ocfn ocfnDonationMinter
              qvfOF     = getFileName ocfn ocfnQVFMainValidator
              dlDatOF   = getFileName ocfn ocfnDeadlineDatum
              initDatOF = getFileName ocfn ocfnInitialGovDatum
              dlSlotOF  = getFileName ocfn ocfnDeadlineSlot
              dlTNOF    = getFileName ocfn ocfnDeadlineTokenNameHex
              --
              govPolicy = Gov.qvfPolicy txRef dl
              govSymbol = mintingPolicyToSymbol govPolicy
              --
              regPolicy = Reg.registrationPolicy govSymbol
              regSymbol = mintingPolicyToSymbol regPolicy
              --
              donPolicy = Don.donationPolicy regSymbol
              donSymbol = mintingPolicyToSymbol donPolicy
              --
              yellow    = "\ESC[38:5:220m"
              red       = "\ESC[38:5:160m"
              green     = "\ESC[38:5:77m"
              purple    = "\ESC[38:5:127m"
              noColor   = "\ESC[0m"

          putStrLn $ yellow ++ "\nGov. Symbol:"
          print govSymbol
          putStrLn $ red ++ "\nReg. Symbol:"
          print regSymbol
          putStrLn $ green ++ "\nDon. Symbol:"
          print donSymbol

          writeTokenNameHex dlTNOF Gov.deadlineTokenName

          dlSlot <- getDeadlineSlot currSlot dl
          govRes <- writeMintingPolicy govOF govPolicy
          regRes <- writeMintingPolicy regOF regPolicy
          donRes <- writeMintingPolicy donOF donPolicy
          case (govRes, regRes, donRes) of
            (Right _, Right _, Right _) -> do
              -- {{{
              let qvfParams =
                    OC.QVFParams
                      { OC.qvfKeyHolder      = fromString pkhStr
                      , OC.qvfSymbol         = govSymbol
                      , OC.qvfProjectSymbol  = regSymbol
                      , OC.qvfDonationSymbol = donSymbol
                      }
              putStrLn $ purple ++ "\nQVF Parameters:"
              print qvfParams
              putStrLn noColor
              valRes <- writeValidator qvfOF $ OC.qvfValidator qvfParams
              case valRes of
                Right _ -> do
                  -- {{{
                  andPrintSuccessWithSize
                    govOF
                    (unsafeParseJSON @OutputPlutus)
                    (return ())
                  andPrintSuccessWithSize
                    regOF
                    (unsafeParseJSON @OutputPlutus)
                    (return ())
                  andPrintSuccessWithSize
                    donOF
                    (unsafeParseJSON @OutputPlutus)
                    (return ())
                  andPrintSuccessWithSize
                    qvfOF
                    (unsafeParseJSON @OutputPlutus)
                    (return ())
                  andPrintSuccess
                    dlSlotOF
                    (LBS.writeFile dlSlotOF $ encode dlSlot)
                  andPrintSuccess dlDatOF $ writeJSON dlDatOF $ deadlineDatum dl
                  andPrintSuccess initDatOF $ writeJSON initDatOF initialGovDatum
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
    "register-project" : txRefStr : pkhStr : lbl : reqFundStr : fileNamesJSON : _              ->
      -- {{{
      case (readTxOutRef txRefStr, readMaybe reqFundStr, A.decode $ fromString fileNamesJSON) of
        (Just utxo, Just reqFund, Just ocfn) ->
          -- {{{
          let
            govRedeemer :: QVFAction
            govRedeemer = RegisterProject

            projDetails :: ProjectDetails
            projDetails =
              ProjectDetails (fromString pkhStr) (fromString lbl) reqFund

            regRedeemer :: Reg.RegistrationRedeemer
            regRedeemer =
              Reg.RegisterProject $ RegistrationInfo utxo projDetails
          in
          actOnCurrentDatum ocfn govRedeemer (Just regRedeemer) $ \case
            RegisteredProjectsCount soFar ->
              -- {{{
              let
                updatedDatum      :: QVFDatum
                updatedDatum      = RegisteredProjectsCount $ soFar + 1
                updatedDatumFile  :: FilePath
                updatedDatumFile  = getFileName ocfn ocfnUpdatedDatum

                newDatum          :: QVFDatum
                newDatum          = ReceivedDonationsCount 0
                newDatumFile      :: FilePath
                newDatumFile      = getFileName ocfn ocfnNewDatum

                projTokenName     :: TokenName
                projTokenName     = orefToTokenName utxo
                projTokenNameFile :: FilePath
                projTokenNameFile = getFileName ocfn ocfnProjectTokenName

                projInfo          :: QVFDatum
                projInfo          = ProjectInfo projDetails
                projInfoFile      :: FilePath
                projInfoFile      =
                     ocfnPreDir ocfn
                  ++ "/"
                  ++ unsafeTokenNameToHex projTokenName
              in do
              andPrintSuccess projTokenNameFile $
                writeTokenNameHex projTokenNameFile projTokenName
              andPrintSuccess updatedDatumFile $
                writeJSON updatedDatumFile updatedDatum
              andPrintSuccess newDatumFile $
                writeJSON newDatumFile newDatum
              andPrintSuccess projInfoFile $
                writeJSON projInfoFile projInfo
              -- }}}
            _                             ->
              -- {{{
              putStrLn "FAILED: Provided current datum is incompatible."
              -- }}}
          -- }}}
        _                                    ->
          -- {{{
          putStrLn $ "FAILED with bad arguments: "
            ++ txRefStr
            ++ " "
            ++ pkhStr
            ++ " "
            ++ lbl
            ++ " "
            ++ reqFundStr
            ++ " "
            ++ fileNamesJSON
          -- }}}
      -- }}}
    "donate-to-project" : pkhStr : projIdStr : amtStr : fileNamesJSON : _                      ->
      -- {{{
      case (hexStringToByteString projIdStr, readMaybe amtStr, A.decode $ fromString fileNamesJSON) of
        (Just projId, Just amt, Just ocfn) ->
          -- {{{
          let
            projId'     :: BuiltinByteString
            projId'     = toBuiltin $ LBS.toStrict projId

            govRedeemer :: QVFAction
            govRedeemer = DonateToProject projId'

            donorPKH    :: Ledger.PubKeyHash
            donorPKH    = fromString pkhStr

            donInfo     :: DonationInfo
            donInfo     = DonationInfo projId' donorPKH amt

            donRedeemer :: Don.DonationRedeemer
            donRedeemer = Don.DonateToProject donInfo
          in
          actOnCurrentDatum ocfn govRedeemer (Just donRedeemer) $ \case
            ReceivedDonationsCount soFar ->
              -- {{{
              let
                updatedDatum      :: QVFDatum
                updatedDatum      = ReceivedDonationsCount $ soFar + 1
                updatedDatumFile  :: FilePath
                updatedDatumFile  = getFileName ocfn ocfnUpdatedDatum

                newDatum          :: QVFDatum
                newDatum          = Donation donorPKH
                newDatumFile      :: FilePath
                newDatumFile      = getFileName ocfn ocfnNewDatum
              in do
              andPrintSuccess updatedDatumFile $
                writeJSON updatedDatumFile updatedDatum
              andPrintSuccess newDatumFile $
                writeJSON newDatumFile newDatum
              -- }}}
            _                            ->
              -- {{{
              putStrLn "FAILED: Provided current datum is incompatible."
              -- }}}
          -- }}}
        _                                  ->
          -- {{{
          putStrLn $ "FAILED with bad arguments: "
            ++ pkhStr
            ++ " "
            ++ projIdStr
            ++ " "
            ++ amtStr
            ++ " "
            ++ fileNamesJSON
          -- }}}
      -- }}}
    "fold-donations" : _                                                                       ->
      putStrLn "TODO."
    "accumulate-donations" : _                                                                 ->
      putStrLn "TODO."
    "collect-key-holder-fee": fileNamesJSON : _                                                ->

      -- {{{
      case A.decode $ fromString fileNamesJSON of

        Just ocfn ->

          -- {{{
          let

            --keyholderPKH    :: Ledger.PubKeyHash
            --keyholderPKH    = fromString pkhStr

            --govRedeemer :: QVFAction
            --govRedeemer = DonateToProject projId'

            --donInfo     :: DonationInfo
            --donInfo     = DonationInfo projId' donorPKH amt

            --donRedeemer :: Don.DonationRedeemer
            --donRedeemer = Don.DonateToProject donInfo

            govRedeemer :: QVFAction
            govRedeemer = PayKeyHolderFee

            --projDetails :: ProjectDetails
            --projDetails =
            --  ProjectDetails (fromString pkhStr) (fromString lbl) reqFund

            --regRedeemer :: Reg.RegistrationRedeemer
            --regRedeemer =
            --  Reg.RegisterProject $ RegistrationInfo utxo projDetails

            --TODO make correct redeemer
            --someRedeemer :: Reg.RegistrationRedeemer
            --someRedeemer =
            --  Reg.RegisterProject $ RegistrationInfo utxo projDetails

          in
          actOnCurrentDatum @QVFAction ocfn govRedeemer Nothing $ \case
            --RegisteredProjectsCount soFar ->
            DonationAccumulationConcluded ps totalLovelaces sumP _ ->

              -- {{{
              let

                --fee = div (fromInteger ttls * 5) 100 -- floor gives remainder to the prize pool
                --updatedPool = ttls - fee

                --(khFee, _) = OC.findDatumAfterPayingKeyHoldersFee ps totalLovelaces sumP
                --updatedPool = totalLovelaces - khFee

                (khFee, updatedDatum) = OC.findDatumAfterPayingKeyHoldersFee ps totalLovelaces sumP
                --updatedDatum      :: QVFDatum
                --updatedDatum      = RegisteredProjectsCount $ soFar + 1
                --updatedDatumFile  :: FilePath
                --updatedDatumFile  = getFileName ocfn ocfnUpdatedDatum

                --updatedDatum      :: QVFDatum
                --updatedDatum      = DonationAccumulationConcluded ps updatedPool sumP True
                updatedDatumFile  :: FilePath
                updatedDatumFile  = getFileName ocfn ocfnUpdatedDatum

                in do
                writeJSON updatedDatumFile updatedDatum
                print khFee
              -- }}}
            _                             ->
              -- {{{
              putStrLn "FAILED: Provided current datum is incompatible."
              -- }}}
          -- }}}
        _                                    ->
          -- {{{
          putStrLn $ "FAILED with bad arguments: "
            ++ fileNamesJSON
          -- }}}
      -- }}}

    "distribute-prize" : _                                                                     ->
      putStrLn "TODO."
    "unlock-bounty-for" : _                                                                    ->
      putStrLn "TODO."
    "withdraw-bounty" : _                                                                      ->
      putStrLn "TODO."
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
    "cbor-to-data" : cborStr : _                                        ->
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
      in
      case mScriptData of
        Just sd ->
          print $ encode $ scriptDataToJson ScriptDataJsonDetailedSchema sd
        Nothing -> do
          putStrLn "FAILED to decode CBOR."
      -- }}}
    "string-to-hex" : tn : outFile : _                                  ->
      -- {{{
      andPrintSuccess outFile $ writeTokenNameHex outFile $ fromString tn
      -- }}}
    "get-deadline-slot" : currSlotStr : datumJSON : _                   -> do
      -- {{{
      case readMaybe currSlotStr of
        Just currSlot -> do
          -- {{{
          actOnData datumJSON $ \case
            DeadlineDatum dlPOSIX -> do
              -- {{{
              dlSlot <- getDeadlineSlot currSlot dlPOSIX
              print dlSlot
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
--           dlTNOF    = getFileName ocfn @"ocfnDeadlineTokenNameHex"
--       ```
--
--       Another solution could be a custom `FromJSON` instance.
--
data OffChainFileNames = OffChainFileNames
  { ocfnPreDir               :: String
  , ocfnDeadlineTokenNameHex :: String
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
getFileName ocfn handle = ocfnPreDir ocfn ++ "/" ++ handle ocfn


data ScriptGenerationArgumentsParseResults =
  ScriptGenerationArgumentsParseResults
    { sgUTxO      :: Maybe Ledger.TxOutRef
    , sgSlot      :: Maybe Integer
    , sgDeadline  :: Maybe Ledger.POSIXTime
    , sgFileNames :: Maybe OffChainFileNames
    }


unwrapScriptGenerationArgs :: ScriptGenerationArgumentsParseResults
                           -> Either String ( Ledger.TxOutRef
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
                                -> (Ledger.TxOutRef -> Integer -> Ledger.POSIXTime -> OffChainFileNames -> IO ())
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
