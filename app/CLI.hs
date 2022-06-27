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
      <> "with the QVF smart contract.\n\n"

      <> "\tGenerate the initial datum:\n"
      <> "\tqvf-cli generate initial-datum <output.json>\n\n"

      <> "\tGenerate the redeemer for adding a project:\n"
      <> "\tqvf-cli generate redeemer AddProject <project-public-key-hash>\n"
      <> "\t                                     <project-label>\n"
      <> "\t                                     <requested-fund>\n"
      <> "\t                                     <output.json>\n\n"

      <> "\tGenerate the redeemer for donating to a project:\n"
      <> "\tqvf-cli generate redeemer Donate <donors-public-key-hash>\n"
      <> "\t                                 <target-projects-public-key-hash>\n"
      <> "\t                                 <donation-amount>\n"
      <> "\t                                 <output.json>\n\n"

      <> "\tGenerate the redeemer for contribution to the pool:\n"
      <> "\tqvf-cli generate redeemer Contribute <contribution-amount>\n"
      <> "\t                                     <output.json>\n\n"

      <> "\tGenerate the redeemer for trigerring the distribution of funds:\n"
      <> "\tqvf-cli generate redeemer Distribute <output.json>\n\n"

      <> "\tGenerate the compiled Plutus script of the validation function:\n"
      <> "\tqvf-cli generate script <key-holders-public-key-hash>\n"
      <> "\t                        <output.json>\n\n"
      -- }}}
    printHelp = putStrLn helpText
  in do
  allArgs <- getArgs
  case allArgs of
    "generate" : "initial-datum" : outFile : _                                     ->
      -- {{{
      writeJSON outFile OC.initialDatum
      -- }}}
    "generate" : "redeemer" : "AddProject" : pPKH : pLabel : pReqStr : outFile : _ ->
      -- {{{
      case readMaybe pReqStr of
        Just pReq ->
          -- {{{
          let
            apParams = OC.AddProjectParams
              { appPubKeyHash = fromString pPKH
              , appLabel      = fromString pLabel
              , appRequested  = pReq
              }
          in
          writeJSON outFile $ OC.AddProject apParams
          -- }}}
        Nothing ->
          -- {{{
          putStrLn "Failed to parse the requested fund."
          -- }}}
      -- }}}
    "generate" : "redeemer" : "Donate" : dDonor : dProject : dAmount : outFile : _ ->
      -- {{{
      case readMaybe dAmount of
        Just amount ->
          -- {{{
          let
            dParams = OC.DonateParams
              { dpDonor   :: fromString dDonor
              , dpProject :: fromString dProject
              , dpAmount  :: amount
              }
          in
          writeJSON outFile $ OC.Donate apParams
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
          writeJSON outFile $ OC.Contribute amount
          -- }}}
        Nothing ->
          -- {{{
          putStrLn "Failed to parse the contribution amount."
          -- }}}
      -- }}}
    "generate" : "redeemer" : "Distribute" : outFile : _                           ->
      -- {{{
      writeJSON outFile OC.Distribute
      -- }}}
    "generate" : "script" : keyHolderPKHStr : outFile : _                          ->
      -- {{{
        writeValidator outFile
      $ OC.qvfValidator
      $ fromString keyHolderPKHStr
      -- }}}
    _ ->
      printHelp
-- }}}
