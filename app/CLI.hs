{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}


module Main (main) where


import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (Serialise, serialise)
-- import           Control.Monad         (forM, foldM)
import qualified Data.Aeson            as A
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           Data.String           (fromString)
import qualified Data.Text             as T
import           Data.Text             (Text)
import           PlutusTx              (Data (..))
import qualified PlutusTx
import           PlutusTx.Monoid       (mempty)
import qualified Ledger
import           System.Environment    (getArgs)
import           Text.Read             (readMaybe)

import qualified OnChain               as OC
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
  . scriptDataToJson ScriptDataJsonDetailedSchema
  . dataToScriptData
  . PlutusTx.toData
  -- }}}


parseJSON :: FilePath -> IO (Either String Data)
parseJSON file = do
  -- {{{
  fileContent <- LBS.readFile file
  case A.decode fileContent of
    Just decoded ->
      case scriptDataFromJson ScriptDataJsonDetailedSchema decoded of
        Right scriptData ->
          return $ Right (scriptDataToData scriptData)
        Left err ->
          return $ Left $ show err
    Nothing ->
      return $ Left "Invalid JSON."
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
-- }}}


-- APPLICATION
-- {{{
main :: IO ()
main =
  let
    helpText :: String
    helpText  =
      -- {{{
         "\nCLI application to generate various redeemer values to interact "
      ++ "with the QVF smart contract.\n\n"

      ++ "Use (-h|--help|man) to print this help text.\n\n\n"


      ++ "\tGenerate the initial datum:\n\n"

      ++ "\tcabal run qvf-cli -- generate      \\\n"
      ++ "\t                     mempty-datum  \\\n"
      ++ "\t                     <output.json>\n\n\n"


      ++ "\tGenerate the compiled Plutus validation and minting script\n"
      ++ "\t(note the UTxO format):\n\n"

      ++ "\tcabal run qvf-cli -- generate                      \\\n"
      ++ "\t                     scripts                       \\\n"
      ++ "\t                     <key-holders-public-key-hash> \\\n"
      ++ "\t                     <txID>#<output-index>         \\\n"
      ++ "\t                     <auth-token-name>             \\\n"
      ++ "\t                     <auth-token-count>            \\\n"
      ++ "\t                     <output-minting.plutus>       \\\n"
      ++ "\t                     <output-validation.plutus>\n\n\n"


      ++ "\tUpdate a given datum by adding a project:\n\n"

      ++ "\tcabal run qvf-cli -- <current-datum.json>      \\\n"
      ++ "\t                     add-project               \\\n"
      ++ "\t                     <project-public-key-hash> \\\n"
      ++ "\t                     <project-label>           \\\n"
      ++ "\t                     <requested-fund>          \\\n"
      ++ "\t                     <output-datum.json>       \\\n"
      ++ "\t                     <output-redeemer.json>\n\n\n"


      ++ "\tUpdate a given datum by donating to a project:\n\n"

      ++ "\tcabal run qvf-cli -- <current-datum.json>              \\\n"
      ++ "\t                     donate                            \\\n"
      ++ "\t                     <donors-public-key-hash>          \\\n"
      ++ "\t                     <target-projects-public-key-hash> \\\n"
      ++ "\t                     <donation-amount>                 \\\n"
      ++ "\t                     <output-datum.json>               \\\n"
      ++ "\t                     <output-redeemer.json>\n\n\n"


      ++ "\tUpdate a given datum by contributing to the pool:\n\n"

      ++ "\tcabal run qvf-cli -- <current-datum.json>   \\\n"
      ++ "\t                     contribute             \\\n"
      ++ "\t                     <contribution-amount>  \\\n"
      ++ "\t                     <output-datum.json>    \\\n"
      ++ "\t                     <output-redeemer.json>\n\n\n"


      ++ "\tGenerate the redeemer for trigerring the distribution of funds:\n\n"

      ++ "\tcabal run qvf-cli -- generate               \\\n"
      ++ "\t                     distribution-redeemer  \\\n"
      ++ "\t                     <output-redeemer.json>\n\n\n"
      -- }}}
    printHelp = putStrLn helpText
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
          putStrLn $ "BAD REDEEMER: " ++ show err
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
  in do
  allArgs <- getArgs
  case allArgs of
    "generate" : "mempty-datum" : outFile : _                                ->
      -- {{{
      andPrintSuccess outFile $ writeJSON outFile Prelude.mempty
      -- }}}
    "generate" : "distribution-redeemer" : outFile : _                       ->
      -- {{{
      andPrintSuccess outFile $ writeJSON outFile OC.Distribute
      -- }}}
    "generate" : "scripts" : pkhStr : txRefStr : tn : amtStr : mOF : vOF : _ -> do
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
                  -- }}}
              -- }}}
          -- }}}
      -- }}}
    "-h"       : _                                                           -> printHelp
    "--help"   : _                                                           -> printHelp
    "man"      : _                                                           -> printHelp
    datumJSON  : restOfArgs                                                  -> do
      -- {{{
      eitherErrData <- parseJSON datumJSON
      case eitherErrData of
        Left parseError ->
          -- {{{
          putStrLn $ "FAILED to parse datum JSON: " ++ parseError
          -- }}}
        Right datumData ->
          -- {{{
          let
            mDatum :: Maybe OC.QVFDatum
            mDatum = PlutusTx.fromData datumData
          in
          case (mDatum, restOfArgs) of
            (Nothing       , _                                                      ) ->
              -- {{{
              putStrLn $ "FAILED: Improper datum."
              -- }}}
            (Just currDatum, "add-project" : pPKH : pLabel : pReqStr : dOF : rOF : _) ->
              -- {{{
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
            (Just currDatum, "donate" : dDonor : dProject : dAmount : dOF : rOF : _ ) ->
              -- {{{
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
            (Just currDatum, "contribute" : amountStr : dOF : rOF : _               ) ->
              -- {{{
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
            _                                                                         ->
              -- {{{
              printHelp
              -- }}}
          -- }}}
      -- }}}
    _ ->
      printHelp
-- }}}
