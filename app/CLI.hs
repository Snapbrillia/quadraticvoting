{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}


module Main where


import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
-- import           Control.Monad         (forM, foldM)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           Data.String           (fromString)
import qualified Data.Text             as T
import           Data.Text             (Text)
import           PlutusTx              (Data (..))
import qualified PlutusTx
import qualified Ledger
import           System.Environment    (getArgs)
import           Text.Read             (readMaybe)

import qualified OnChain               as OC

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

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file =
  -- {{{
    LBS.writeFile file
  . encode
  . scriptDataToJson ScriptDataJsonDetailedSchema
  . dataToScriptData
  . PlutusTx.toData
  -- }}}


writeValidator :: FilePath
               -> Ledger.Validator
               -> IO (Either (FileError ()) ())
writeValidator file =
  -- {{{
    writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing
  . PlutusScriptSerialised
  . SBS.toShort
  . LBS.toStrict
  . serialise
  . Ledger.unValidatorScript
  -- }}}
-- }}}


-- Contract Monad Implementation
--   - Tasos
--   - Keyan
--   - Andy
-- Multiple Emulator Trace Scenarios
--   - Curtis
--   - Shan
-- CLI Application Implementation
--   - Tasos
--   - Keyan
-- Bash Scripts
--   - Tasos
--   - Shan
--   - Paul


-- APPLICATION
-- {{{
main :: IO ()
main =
  let
    helpText  =
      -- {{{
         "\nCLI application to generate various redeemer values to interact "
      ++ "with the QVF smart contract.\n\n"

      ++ "\tGenerate the initial datum:\n\n"
      ++ "\tcabal run qvf-cli -- generate      \\\n"
      ++ "\t                     initial-datum \\\n"
      ++ "\t                     <output.json>\n\n\n"


      ++ "\tGenerate the redeemer for adding a project:\n\n"

      ++ "\tcabal run qvf-cli -- generate                  \\\n"
      ++ "\t                     redeemer                  \\\n"
      ++ "\t                     AddProject                \\\n"
      ++ "\t                     <project-public-key-hash> \\\n"
      ++ "\t                     <project-label>           \\\n"
      ++ "\t                     <requested-fund>          \\\n"
      ++ "\t                     <output.json>\n\n\n"


      ++ "\tGenerate the redeemer for donating to a project:\n\n"

      ++ "\tcabal run qvf-cli -- generate                          \\\n"
      ++ "\t                     redeemer                          \\\n"
      ++ "\t                     Donate                            \\\n"
      ++ "\t                     <donors-public-key-hash>          \\\n"
      ++ "\t                     <target-projects-public-key-hash> \\\n"
      ++ "\t                     <donation-amount>                 \\\n"
      ++ "\t                     <output.json>\n\n\n"


      ++ "\tGenerate the redeemer for contribution to the pool:\n\n"

      ++ "\tcabal run qvf-cli -- generate              \\\n"
      ++ "\t                     redeemer              \\\n"
      ++ "\t                     Contribute            \\\n"
      ++ "\t                     <contribution-amount> \\\n"
      ++ "\t                     <output.json>\n\n\n"


      ++ "\tGenerate the redeemer for trigerring the distribution of funds:\n\n"

      ++ "\tcabal run qvf-cli -- generate      \\\n"
      ++ "\t                     redeemer      \\\n"
      ++ "\t                     Distribute    \\\n"
      ++ "\t                     <output.json>\n\n\n"


      ++ "\tGenerate the compiled Plutus script of the validation function:\n\n"

      ++ "\tcabal run qvf-cli -- generate                      \\\n"
      ++ "\t                     script                        \\\n"
      ++ "\t                     <key-holders-public-key-hash> \\\n"
      ++ "\t                     <output.json>\n\n\n"
      -- }}}
    printHelp = putStrLn helpText
    andPrintSuccess outFile ioAction = do
      -- {{{
      ioAction
      putStrLn $ outFile ++ " generated SUCCESSFULLY."
      -- }}}
  in do
  allArgs <- getArgs
  case allArgs of
    "generate" : "initial-datum" : outFile : _                                     ->
      -- {{{
      andPrintSuccess outFile $ writeJSON outFile OC.initialDatum
      -- }}}
    "generate" : "redeemer" : "AddProject" : pPKH : pLabel : pReqStr : outFile : _ ->
      -- {{{
      case readMaybe pReqStr of
        Just pReq ->
          -- {{{
          let
            apParams = OC.AddProjectParams
              { OC.appPubKeyHash = fromString pPKH
              , OC.appLabel      = fromString pLabel
              , OC.appRequested  = pReq
              }
          in
          andPrintSuccess outFile $ writeJSON outFile $ OC.AddProject apParams
          -- }}}
        Nothing ->
          -- {{{
          putStrLn "FAILED to parse the requested fund."
          -- }}}
      -- }}}
    "generate" : "redeemer" : "Donate" : dDonor : dProject : dAmount : outFile : _ ->
      -- {{{
      case readMaybe dAmount of
        Just amount ->
          -- {{{
          let
            dParams = OC.DonateParams
              { OC.dpDonor   = fromString dDonor
              , OC.dpProject = fromString dProject
              , OC.dpAmount  = amount
              }
          in
          andPrintSuccess outFile $ writeJSON outFile $ OC.Donate dParams
          -- }}}
        Nothing ->
          -- {{{
          putStrLn "Failed to parse the donation amount."
          -- }}}
      -- }}}
    "generate" : "redeemer" : "Contribute" : amountStr : outFile : _               ->
      -- {{{
      case readMaybe amountStr of
        Just amount ->
          -- {{{
          andPrintSuccess outFile $ writeJSON outFile $ OC.Contribute amount
          -- }}}
        Nothing ->
          -- {{{
          putStrLn "Failed to parse the contribution amount."
          -- }}}
      -- }}}
    "generate" : "redeemer" : "Distribute" : outFile : _                           ->
      -- {{{
      andPrintSuccess outFile $ writeJSON outFile OC.Distribute
      -- }}}
    "generate" : "script" : keyHolderPKHStr : outFile : _                          -> do
      -- {{{
      eitherFEUnit <-   writeValidator outFile
                      $ OC.qvfValidator
                      $ fromString keyHolderPKHStr
      case eitherFEUnit of
        Left _ ->
          putStrLn "Failed to write Plutus script file."
        Right _ ->
          andPrintSuccess outFile return
      -- }}}
    _ ->
      printHelp
-- }}}
