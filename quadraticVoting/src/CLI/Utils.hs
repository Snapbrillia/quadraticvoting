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
import qualified Data.Aeson                  as A
import           Data.Aeson                  ( encode )
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Short       as SBS
import qualified Data.Char                   as Char
import           Data.Foldable               ( forM_ )
import           Data.Maybe                  ( fromJust )
import           Data.String                 ( fromString )
import           Data.Text                   ( Text )
import           Data.Time.Clock.POSIX       ( getPOSIXTime )
import           Data.Word                   ( Word8 )
import           GHC.Generics                ( Generic )
import           Plutus.V1.Ledger.Api        ( fromBuiltin
                                             , toBuiltin
                                             , BuiltinByteString )
import qualified Plutus.V1.Ledger.Credential as Plutus
import qualified Plutus.V1.Ledger.Crypto     as Plutus
import           Plutus.V1.Ledger.Value      ( TokenName(..) )
import           Plutus.V2.Ledger.Api        ( TxOutRef(..)
                                             , ToData(..) )
import qualified Plutus.V2.Ledger.Api        as Ledger
import           PlutusTx                    ( Data (..) )
import qualified PlutusTx
import qualified PlutusTx.AssocMap           as Map
import           PlutusTx.Prelude            ( lengthOfByteString )
import           Prelude

import qualified CLI.OffChainFileNames       as OCFN
import           CLI.OffChainFileNames       ( OffChainFileNames )
import           Data.Datum
import           Data.Redeemer
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


withSuccessMessage :: FilePath -> IO () -> IO ()
withSuccessMessage outFile ioAction = do
  -- {{{
  ioAction
  putStrLn $ outFile ++ " generated SUCCESSFULLY."
  -- }}}


withSuccessMessageAndSize :: Show a
                          => FilePath
                          -> (FilePath -> IO a)
                          -> IO ()
                          -> IO ()
withSuccessMessageAndSize outFile getByteCount ioAction = do
  -- {{{
  ioAction
  printable <- getByteCount outFile
  let byteCount = show printable
  putStrLn $
    outFile ++ " generated SUCCESSFULLY (" ++ byteCount ++ " bytes)."
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
    "DistributionProgress"          ->
      Just $ \case
        DistributionProgress {} -> True
        _                       -> False
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
  let datumJSON       = OCFN.currentDatum ocfn
      qvfRedeemerFile = OCFN.qvfRedeemer  ocfn
  writeJSON qvfRedeemerFile qvfRedeemer
  forM_ mMinterRedeemer $ writeJSON (OCFN.minterRedeemer ocfn)
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
      case decodeString ocfnStr of
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
      case decodeString ocfnStr of
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
      case decodeString ocfnStr of
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


decodeString :: A.FromJSON a => String -> Maybe a
decodeString = A.decode . fromString

-- EXTRA
-- {{{
data OutputPlutus = OutputPlutus
  { cborHex :: BuiltinByteString
  } deriving (Generic, A.ToJSON, A.FromJSON)
instance Show OutputPlutus where
  show OutputPlutus{..} =
    show $ lengthOfByteString cborHex


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


mkRGBColor :: Word8 -> Word8 -> Word8 -> String
mkRGBColor r g b =
  "\ESC[38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"
yellow  :: String
yellow  = mkRGBColor 252 209 47
red     :: String
red     = mkRGBColor 239 76  40
green   :: String
green   = mkRGBColor 25  176 92
purple  :: String
purple  = mkRGBColor 155 39  255
noColor :: String
noColor = "\ESC[0m"
-- }}}




