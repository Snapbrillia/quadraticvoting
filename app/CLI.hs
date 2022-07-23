{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}


module Main (main) where


import           Cardano.Api
import           Cardano.Api.Shelley    (PlutusScript (..))
import           Codec.Serialise        (Serialise, serialise)
import           Control.Monad          (foldM)
import qualified Data.Aeson             as A
import           Data.Aeson             (encode)
import qualified Data.ByteString.Char8  as BS8
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Short  as SBS
import qualified Data.List              as List
import           Data.Maybe             (fromJust)
import           Data.String            (fromString)
import qualified Data.Text              as T
import           Data.Text              (Text)
import           Plutus.V1.Ledger.Api   (fromBuiltin)
import           Plutus.V1.Ledger.Value (TokenName (..))
import           PlutusTx               (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Monoid        as PlutusMonoid
import qualified PlutusTx.Semigroup     as PlutusSemigroup
import qualified Ledger
import           System.Environment     (getArgs)
import           Text.Read              (readMaybe)

import qualified OnChain                as OC
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


parseJSON :: FilePath -> IO (Either String Data)
parseJSON file = do
  -- {{{
  fileContent <- LBS.readFile file
  parseJSONValue fileContent
  -- }}}


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


-- | From PPP's 6th lecture.
unsafeTokenNameToHex :: TokenName -> String
unsafeTokenNameToHex =
  -- {{{
    BS8.unpack
  . serialiseToRawBytesHex
  . fromJust
  . deserialiseFromRawBytes AsAssetName
  . fromBuiltin
  . unTokenName
  -- }}}
-- }}}


-- APPLICATION
-- {{{
main :: IO ()
main =
  let
    helpCurrDatum     :: String
    helpCurrDatum     = "<current.datum>"
    helpUpdatedDatum  :: String
    helpUpdatedDatum  = "<updated.datum>"
    helpRedeemer      :: String
    helpRedeemer      = "<action.redeemer>"
    commonLastThree   = [helpCurrDatum, helpUpdatedDatum, helpRedeemer]
    makeHelpText :: String -> String -> String -> [String] -> String
    makeHelpText description elem0 elem1 restOfElems =
      -- {{{
      let
        elems           = elem0 : elem1 : restOfElems
        cmd             :: String
        cmd             = "cabal run qvf-cli -- "
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
    toHexHelp         :: String
    toHexHelp         =
      -- {{{
      makeHelpText
        "Export the hex serialization of a token name:"
        "string-to-hex"
        "<token-name>"
        ["<output.hex>"]
      -- }}}
    datumHasProjHelp  :: String
    datumHasProjHelp  =
      -- {{{
      makeHelpText
        (    "Check if a given datum can be used for donation to a\n"
          ++ "\tspecific project:"
        )
        "datum-has-project"
        "{current-datum-json-value}"
        ["<project.pkh>"]
      -- }}}
    prettyDatumHelp   :: String
    prettyDatumHelp   =
      -- {{{
      makeHelpText
        "Print an easy-to-read parsing of a given datum JSON:"
        "pretty-datum"
        "{current-datum-json-value}"
        []
      -- }}}
    mergeDatumsHelp   :: String
    mergeDatumsHelp   =
      -- {{{
      makeHelpText
        "Combine multiple states, and print contract's merged state:"
        "merge-datums"
        "{datum0-json-value}"
        [ "{datum0-json-value}"
        , "{datum1-json-value}"
        , "{datum2-json-value}"
        , "{etc...}"
        ]
      -- }}}
    scriptHelp        :: String
    scriptHelp        =
      -- {{{
      makeHelpText
        (    "Generate the compiled Plutus validation and minting script\n"
          ++ "\t(note the UTxO format):"
        )
        "generate"
        "scripts"
        [ "<key-holder.pkh>"
        , "<txID>#<output-index>"
        , "<auth-token-name>"
        , "<auth-token-count>"
        , "<output-minting.plutus>"
        , "<output-validation.plutus>"
        , "<output-NotStarted.datum>"
        , "<output-InitiateFund.redeemer>"
        , "<output-mempty.datum>"
        ]
      -- }}}
    distributeHelp    :: String
    distributeHelp    =
      -- {{{
      makeHelpText
        "Generate the redeemer for trigerring the distribution of funds:"
        "generate"
        "distribution-redeemer"
        [helpRedeemer]
      -- }}}
    addProjectHelp    :: String
    addProjectHelp    =
      -- {{{
      makeHelpText
       "Update a given datum by adding a project:"
       "add-project"
       "<project-public-key-hash>" $
       [ "<project-label>"
       , "<requested-fund>"
       ] ++ commonLastThree
      -- }}}
    donateHelp        :: String
    donateHelp        =
      -- {{{
      makeHelpText
        "Update a given datum by donating to a project:"
        "donate"
        "<donors-public-key-hash>" $
        [ "<target-projects-public-key-hash>"
        , "<donation-amount>"
        ] ++ commonLastThree
      -- }}}
    contributeHelp    :: String
    contributeHelp    =
      -- {{{
      makeHelpText
        "Update a given datum by contributing to the pool:"
        "contribute"
        "<contribution-amount>"
        commonLastThree
      -- }}}
    setDeadlineHelp   :: String
    setDeadlineHelp   =
      -- {{{
      makeHelpText
        "Update a given datum by setting a new deadline:"
        "set-deadline"
        "<new-deadline>"
        commonLastThree
      -- }}}
    unsetDeadlineHelp :: String
    unsetDeadlineHelp =
      -- {{{
      makeHelpText
        "Update a given datum by removing its deadline:"
        "unset-deadline"
        helpCurrDatum
        [ helpUpdatedDatum
        , helpRedeemer
        ]
      -- }}}
    helpText          :: String
    helpText          =
      -- {{{
         "\nCLI application to generate various redeemer values to interact "
      ++ "with the QVF smart contract.\n\n"

      ++ "You can separately print the argument guide for each action\n"
      ++ "with (-h|--help|man) following the desired action. Available\n"
      ++ "options are:\n\n"

      ++ "Primary interactions:\n"
      ++ "\tcabal run qvf-cli -- add-project --help\n"
      ++ "\tcabal run qvf-cli -- donate      --help\n"
      ++ "\tcabal run qvf-cli -- contribute  --help\n\n"

      ++ "Utility:\n"
      ++ "\tcabal run qvf-cli -- generate scripts  --help\n"
      ++ "\tcabal run qvf-cli -- datum-has-project --help\n"
      ++ "\tcabal run qvf-cli -- pretty-datum      --help\n"
      ++ "\tcabal run qvf-cli -- merge-datums      --help\n"
      ++ "\tcabal run qvf-cli -- string-to-hex     --help\n\n"

      ++ "Limited to key holder:\n"
      ++ "\tcabal run qvf-cli -- set-deadline                   --help\n"
      ++ "\tcabal run qvf-cli -- unset-deadline                 --help\n"
      ++ "\tcabal run qvf-cli -- generate distribution-redeemer --help\n\n"

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
    fromAction action currDatum mDOF rOF =
      -- {{{
      case OC.updateDatum action currDatum of
        Left err       ->
          -- {{{
          putStrLn $ "FAILED: Bad redeemer, " ++ show err
          -- }}}
        Right newDatum ->
          -- {{{
          let
            writeRedeemer = andPrintSuccess rOF $ writeJSON rOF action
          in
          case mDOF of
            Just dOF -> do
              andPrintSuccess dOF $ writeJSON dOF newDatum
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
        "distribution-redeemer" ->
          putStrLn distributeHelp
        _                       ->
          printHelp
      -- }}}
    printActionHelp :: String -> IO ()
    printActionHelp action =
      -- {{{
      case action of
        "add-project"       ->
          putStrLn addProjectHelp
        "donate"            ->
          putStrLn donateHelp
        "contribute"        ->
          putStrLn contributeHelp
        "set-deadline"      ->
          putStrLn setDeadlineHelp
        "unset-deadline"    ->
          putStrLn unsetDeadlineHelp
        "distribute"        ->
          putStrLn distributeHelp
        "string-to-hex"     ->
          putStrLn toHexHelp
        "datum-has-project" ->
          putStrLn datumHasProjHelp
        "pretty-datum"      ->
          putStrLn prettyDatumHelp
        "merge-datums"      ->
          putStrLn mergeDatumsHelp
        _                   ->
          printHelp
      -- }}}
    fromDatumValueHelper :: LBS.ByteString
                         -> (String -> IO a)
                         -> IO a
                         -> (OC.QVFDatum -> IO a)
                         -> IO a
    fromDatumValueHelper datumVal parseFailureAction badDatumAction datumToIO = do
      -- {{{
      eitherErrData <- parseJSONValue datumVal
      case eitherErrData of
        Left parseError ->
          -- {{{
          parseFailureAction parseError
          -- }}}
        Right datumData ->
          -- {{{
          let
            mDatum :: Maybe OC.QVFDatum
            mDatum = PlutusTx.fromData datumData
          in
          maybe badDatumAction datumToIO mDatum
          -- }}}
      -- }}}
    fromDatumValue :: LBS.ByteString -> (OC.QVFDatum -> IO ()) -> IO ()
    fromDatumValue datumVal =
      -- {{{
      fromDatumValueHelper
        datumVal
        ( \parseError ->
            putStrLn $ "FAILED to parse datum JSON: " ++ parseError
        )
        (putStrLn $ "FAILED: Improper datum.")
      -- }}}
    fromDatum :: String -> (OC.QVFDatum -> IO ()) -> IO ()
    fromDatum datumJSON datumToIO = do
      -- {{{
      datumVal <- LBS.readFile datumJSON
      fromDatumValue datumVal datumToIO
      -- }}}
  in do
  allArgs <- getArgs
  case allArgs of
    "generate" : genStr : "-h"     : _ -> printGenerateHelp genStr
    "generate" : genStr : "--help" : _ -> printGenerateHelp genStr
    "generate" : genStr : "man"    : _ -> printGenerateHelp genStr
    actionStr  : "-h"     : _          -> printActionHelp actionStr
    actionStr  : "--help" : _          -> printActionHelp actionStr
    actionStr  : "man"    : _          -> printActionHelp actionStr
    "-h"       : _                     -> printHelp
    "--help"   : _                     -> printHelp
    "man"      : _                     -> printHelp
    "generate" : "scripts" : pkhStr : txRefStr : tn : amtStr : mOF : vOF : fstDatOF : initRedOF : distDatOF : _ -> do
      -- {{{
      case (readTxOutRef txRefStr, readMaybe amtStr) of
        (Nothing, _)           ->
          -- {{{
          putStrLn "FAILED to parse the given UTxO."
          -- }}}
        (_, Nothing)           ->
          -- {{{
          putStrLn "FAILED to parse the token amount."
          -- }}}
        (Just txRef, Just amt) -> do
          -- {{{
          let policyParams =
                Token.PolicyParams
                  { Token.ppORef   = txRef
                  , Token.ppToken  = fromString tn
                  , Token.ppAmount = amt
                  }
          mintRes <- writeMintingPolicy mOF $ Token.qvfPolicy policyParams
          case mintRes of
            Left _  ->
              -- {{{
              putStrLn "FAILED to write minting script file."
              -- }}}
            Right _ -> do
              -- {{{
              let tokenSymbol = Token.qvfSymbol policyParams
                  qvfParams   =
                    OC.QVFParams
                      { OC.qvfKeyHolder  = fromString pkhStr
                      , OC.qvfSymbol     = tokenSymbol
                      , OC.qvfTokenName  = fromString tn
                      , OC.qvfTokenCount = amt
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
                  andPrintSuccess fstDatOF
                    $ writeJSON fstDatOF OC.NotStarted
                  andPrintSuccess initRedOF
                    $ writeJSON initRedOF OC.InitiateFund
                  andPrintSuccess distDatOF
                    $ writeJSON distDatOF
                    $ OC.InProgress PlutusMonoid.mempty
                  -- }}}
              -- }}}
          -- }}}
      -- }}}
    "generate" : "distribution-redeemer" : outFile : _                  ->
      -- {{{
      andPrintSuccess outFile $ writeJSON outFile OC.Distribute
      -- }}}
    "datum-has-project" : datumJSONStr : pPKH : _                       ->
      -- {{{
      fromDatumValue (fromString datumJSONStr) $ \givenDatum ->
        let
          pkh              :: Ledger.PubKeyHash
          pkh              = fromString pPKH
          fromQVFInfo info =
            -- {{{
            let
              mProject =
                -- {{{
                List.find
                  ((== pkh) . OC.pPubKeyHash)
                  (OC.qvfProjects info)
                -- }}}
            in
            case mProject of
              Nothing ->
                putStrLn "False"
              Just _  ->
                putStrLn "True"
            -- }}}
        in
        case givenDatum of
          OC.NotStarted      -> putStrLn "False"
          OC.Closed     info -> fromQVFInfo info
          OC.InProgress info -> fromQVFInfo info
      -- }}}
    "pretty-datum" : datumJSONStr : _                                   ->
      -- {{{
      fromDatumValue (fromString datumJSONStr) print
      -- }}}
    "merge-datums" : datumJSONStrs                                      ->
      -- {{{
      let
        foldFn :: Either String OC.QVFDatum
               -> String
               -> IO (Either String OC.QVFDatum)
        foldFn eith datumJSONStr =
          -- {{{
          case eith of
            Left err ->
              return $ Left err
            Right soFar ->
              fromDatumValueHelper
                (fromString datumJSONStr)
                (return . Left)
                (return $ Left "Bad datum encountered.")
                (\qvfDatum -> return $ Right $ qvfDatum PlutusSemigroup.<> soFar)
          -- }}}
      in do
      eithMerged <- foldM foldFn (Right PlutusMonoid.mempty) datumJSONStrs
      case eithMerged of
        Left err     -> putStrLn $ "FAILED: " ++ err
        Right merged -> print merged
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
      fromDatum datumJSON $ \currDatum ->
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
            fromAction action currDatum (Just dOF) rOF
            -- }}}
      -- }}}
    "donate" : dDonor : dProject : dAmount : datumJSON : dOF : rOF : _  ->
      -- {{{
      fromDatum datumJSON $ \currDatum ->
        case readMaybe dAmount of
          Nothing ->
            -- {{{
            putStrLn "FAILED to parse the donation amount."
            -- }}}
          Just amount ->
            -- {{{
            let
              action = OC.Donate $ OC.DonateParams
                { OC.dpDonor   = fromString dDonor
                , OC.dpProject = fromString dProject
                , OC.dpAmount  = amount
                }
            in
            fromAction action currDatum (Just dOF) rOF
            -- }}}
      -- }}}
    "contribute" : amountStr : datumJSON : dOF : rOF : _                ->
      -- {{{
      fromDatum datumJSON $ \currDatum ->
        case readMaybe amountStr of
          Nothing ->
            -- {{{
            putStrLn "FAILED to parse the contribution amount."
            -- }}}
          Just amount ->
            -- {{{
            fromAction (OC.Contribute amount) currDatum (Just dOF) rOF
            -- }}}
      -- }}}
    "set-deadline" : deadlineStr : datumJSON : dOF : rOF : _            ->
      -- {{{
      fromDatum datumJSON $ \currDatum ->
        case Ledger.POSIXTime <$> readMaybe deadlineStr of
          Nothing ->
            -- {{{
            putStrLn "FAILED to parse the new deadline."
            -- }}}
          Just deadline ->
            -- {{{
            fromAction (OC.SetDeadline $ Just deadline) currDatum (Just dOF) rOF
            -- }}}
      -- }}}
    "unset-deadline" : datumJSON : dOF : rOF : _                        ->
      -- {{{
      fromDatum datumJSON $ \currDatum ->
        fromAction
          (OC.SetDeadline Nothing)
          currDatum
          (Just dOF)
          rOF
      -- }}}
    _              -> printHelp
-- }}}
