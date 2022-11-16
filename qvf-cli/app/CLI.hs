{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE LambdaCase        #-}


module Main (main) where


-- IMPORTS
-- {{{
-- import Debug.Trace (trace)

import           Cardano.Api
import qualified Data.Aeson                 as A
import           Data.Aeson                 ( encode )
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Char                  as Char
import           Data.Foldable              ( forM_ )
import           Data.String                ( fromString )
import           Data.Word                  ( Word8 )
import           Plutus.V1.Ledger.Api       ( toBuiltin
                                            , BuiltinByteString )
import           Plutus.V1.Ledger.Value     ( TokenName(..) )
import qualified Plutus.V2.Ledger.Api       as Ledger
import           PlutusTx                   ( Data (..) )
import qualified PlutusTx
import qualified PlutusTx.AssocMap          as Map
import           PlutusTx.AssocMap          ( Map )
import           System.Environment         ( getArgs )
import           Text.Read                  ( readMaybe )

import           Data.Datum
import           Data.DonationInfo
import           Data.Redeemer

import           CLI.Utils
import qualified QVF                        as OC
import qualified Minter.Donation            as Don
import qualified Minter.Governance          as Gov
import qualified Minter.Registration        as Reg
import           Utils
-- }}}


-- APPLICATION
-- {{{
main :: IO ()
main = do
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
          let pkh       = fromString pkhStr
              govOF     = getFileName ocfn ocfnGovernanceMinter
              regOF     = getFileName ocfn ocfnRegistrationMinter
              donOF     = getFileName ocfn ocfnDonationMinter
              qvfOF     = getFileName ocfn ocfnQVFMainValidator
              dlDatOF   = getFileName ocfn ocfnDeadlineDatum
              initDatOF = getFileName ocfn ocfnInitialGovDatum
              dlSlotOF  = getFileName ocfn ocfnDeadlineSlot
              redOF     = getFileName ocfn ocfnMinterRedeemer
              --
              govPolicy = Gov.qvfPolicy pkh txRef dl
              govSymbol = mintingPolicyToSymbol govPolicy
              --
              regPolicy = Reg.registrationPolicy pkh govSymbol
              regSymbol = mintingPolicyToSymbol regPolicy
              --
              donPolicy = Don.donationPolicy pkh regSymbol
              donSymbol = mintingPolicyToSymbol donPolicy
              --
              mkRGBColor :: Word8 -> Word8 -> Word8 -> String
              mkRGBColor r g b =
                "\ESC[38;2;"
                ++ show r ++ ";"
                ++ show g ++ ";"
                ++ show b ++ "m"
              yellow    = mkRGBColor 252 209 47
              red       = mkRGBColor 239 76  40
              green     = mkRGBColor 25  176 92
              purple    = mkRGBColor 155 39  255
              noColor   = "\ESC[0m"

          putStrLn $ yellow ++ "\nGov. Symbol:"
          print govSymbol
          putStrLn $ red ++ "\nReg. Symbol:"
          print regSymbol
          putStrLn $ green ++ "\nDon. Symbol:"
          print donSymbol

          dlSlot <- getDeadlineSlot currSlot dl
          govRes <- writeMintingPolicy govOF govPolicy
          regRes <- writeMintingPolicy regOF regPolicy
          donRes <- writeMintingPolicy donOF donPolicy
          case (govRes, regRes, donRes) of
            (Right _, Right _, Right _) -> do
              -- {{{
              let qvfParams =
                    OC.QVFParams
                      { OC.qvfKeyHolder      = pkh
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
                  andPrintSuccess redOF $ writeJSON redOF Gov.Initiate
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
    "register-project"       : pkhStr : lbl : reqFundStr : fileNamesJSON : _                   ->
      -- {{{
      case (readMaybe reqFundStr, A.decode $ fromString fileNamesJSON) of
        (Just reqFund, Just ocfn) ->
          -- {{{
          let
            govRedeemer :: QVFRedeemer
            govRedeemer = RegisterProject

            projDetails :: ProjectDetails
            projDetails =
              ProjectDetails (fromString pkhStr) (fromString lbl) reqFund

            regRedeemer :: Reg.RegistrationRedeemer
            regRedeemer = Reg.RegisterProject projDetails
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
                projTokenName     = indexToTokenName soFar
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
        _                         ->
          -- {{{
          putStrLn $ "FAILED with bad arguments: "
            ++ pkhStr
            ++ " "
            ++ lbl
            ++ " "
            ++ reqFundStr
            ++ " "
            ++ fileNamesJSON
          -- }}}
      -- }}}
    "donate-to-project"      : pkhStr : projIDStr : amtStr : fileNamesJSON : _                 ->
      -- {{{
      case (hexStringToByteString projIDStr, readMaybe amtStr, A.decode $ fromString fileNamesJSON) of
        (Just projID, Just amt, Just ocfn) ->
          -- {{{
          let
            projID'     :: BuiltinByteString
            projID'     = toBuiltin $ LBS.toStrict projID

            govRedeemer :: QVFRedeemer
            govRedeemer = DonateToProject

            donorPKH    :: Ledger.PubKeyHash
            donorPKH    = fromString pkhStr

            donInfo     :: DonationInfo
            donInfo     = DonationInfo projID' donorPKH amt

            donRedeemer :: Don.DonationRedeemer
            donRedeemer = Don.DonateToProject donInfo
          in
          actOnCurrentDatum ocfn govRedeemer (Just donRedeemer) $ \case
            ReceivedDonationsCount soFar ->
              -- {{{
              let
                updatedDatum     :: QVFDatum
                updatedDatum     = ReceivedDonationsCount $ soFar + 1
                updatedDatumFile :: FilePath
                updatedDatumFile = getFileName ocfn ocfnUpdatedDatum

                newDatum         :: QVFDatum
                newDatum         = Donation donorPKH
                newDatumFile     :: FilePath
                newDatumFile     = getFileName ocfn ocfnNewDatum
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
            ++ projIDStr
            ++ " "
            ++ amtStr
            ++ " "
            ++ fileNamesJSON
          -- }}}
      -- }}}
    "fold-donations"         : restOfArgs                                                      ->
      -- {{{
      let
        go :: String
           -> String
           -> String
           -> (Integer, Integer, Map Ledger.PubKeyHash Integer)
           -> Either String (Integer, Integer, Map Ledger.PubKeyHash Integer)
        go
          lovelacesStr
          donCountStr
          datumStr
          (lCountSoFar, dCountSoFar, dMap) =
            -- {{{
            let
              mLovelaces :: Maybe Integer
              mLovelaces = readMaybe lovelacesStr
              mDonCount  :: Maybe Integer
              mDonCount  = readMaybe donCountStr
              mDatum     :: Maybe QVFDatum
              mDatum     = readDatum datumStr
            in
            case (mLovelaces, mDonCount, mDatum) of
              (Just lovelaces, Just donCount, Just donDatum) ->
                -- {{{
                let
                  helperFn m =
                    ( lCountSoFar + lovelaces
                    , dCountSoFar + donCount
                    , Map.unionWith (+) dMap m
                    )
                in
                case donDatum of
                  Donation pkh    ->
                    Right $ helperFn $ Map.singleton pkh lovelaces
                  Donations dMap' ->
                    Right $ helperFn dMap'
                  _               ->
                    Left "Invalid donation UTxO provided."
                -- }}}
              _                                                                       ->
                -- {{{
                Left $ "Bad arguments for a donation(s) UTxO: "
                  ++ lovelacesStr
                  ++ " "
                  ++ donCountStr
                  ++ " "
                  ++ datumStr
                -- }}}
            -- }}}
        initEith = Right (0, 0, Map.empty)
        eith = infArgHelper3 go initEith restOfArgs
      in
      case eith of
        Right ((inputLovelaces, inputDonations, foldedMap), ocfn) ->
          -- {{{
          actOnCurrentDatum @QVFRedeemer ocfn FoldDonations Nothing $ \currDatum ->
            let
              updatedDatumFile    :: FilePath
              updatedDatumFile    = getFileName ocfn ocfnUpdatedDatum
              newDatumFile        :: FilePath
              newDatumFile        = getFileName ocfn ocfnNewDatum
              newDatum            :: QVFDatum
              newDatum            = Donations foldedMap
              jsonToPrint ls mint =
                   "{\"lovelace\":\"" ++ show ls
                ++ "\",\"mint\":\""   ++ show (negate mint)
                ++ "\"}"
            in
            case currDatum of
              ReceivedDonationsCount soFar      ->
                -- {{{
                let
                  (updatedDatum, ls, mintAmt) =
                    -- {{{
                    if inputDonations == soFar then
                      ( PrizeWeight (Don.foldDonationsMap foldedMap) False
                      , inputLovelaces + halfOfTheRegistrationFee
                      , soFar
                      )
                    else
                      ( DonationFoldingProgress soFar inputDonations
                      , inputLovelaces
                      , 0
                      )
                    -- }}}
                in do
                writeJSON updatedDatumFile updatedDatum
                writeJSON newDatumFile newDatum
                -- forM_ mNew (writeJSON newDatumFile)
                putStrLn $ jsonToPrint ls mintAmt
                -- }}}
              DonationFoldingProgress tot soFar ->
                -- {{{
                let
                  newSoFar                    =
                    if soFar + inputDonations > tot then
                      inputDonations
                    else
                      soFar + inputDonations
                  (updatedDatum, ls, mintAmt) =
                    -- {{{
                    if newSoFar == tot then
                      ( PrizeWeight (Don.foldDonationsMap foldedMap) False
                      , inputLovelaces + halfOfTheRegistrationFee
                      , tot
                      )
                    else
                      ( DonationFoldingProgress tot newSoFar
                      , inputLovelaces
                      , 0
                      )
                    -- }}}
                in do
                writeJSON updatedDatumFile updatedDatum
                writeJSON newDatumFile newDatum
                putStrLn $ jsonToPrint ls mintAmt
                -- }}}
              _                                 ->
                -- {{{
                putStrLn "FAILED: Bad project UTxO provided."
                -- }}}
          -- }}}
        Left errMsg                                               ->
          -- {{{
          putStrLn $ "FAILED: " ++ errMsg
          -- }}}
      -- }}}
    "consolidate-donations"  : restOfArgs                                                      ->
      -- {{{
      let
        go :: String
           -> String
           -> String
           -> (Integer, Integer, Integer)
           -> Either String (Integer, Integer, Integer)
        go
          lovelacesStr
          donCountStr
          datumStr
          (lCountSoFar, dCountSoFar, wsSoFar) =
            -- {{{
            let
              mLovelaces :: Maybe Integer
              mLovelaces = readMaybe lovelacesStr
              mDonCount  :: Maybe Integer
              mDonCount  = readMaybe donCountStr
              mDatum     :: Maybe QVFDatum
              mDatum     = readDatum datumStr
            in
            case (mLovelaces, mDonCount, mDatum) of
              (Just lovelaces, Just donCount, Just donDatum) ->
                -- {{{
                case donDatum of
                  Donations dMap' ->
                    -- {{{
                    Right
                      ( lCountSoFar + lovelaces
                      , dCountSoFar + donCount
                      , wsSoFar     + Don.sumSquareRoots dMap'
                      )
                    -- }}}
                  _               ->
                    -- {{{
                    Left "Invalid donation UTxO provided."
                    -- }}}
                -- }}}
              _                                                                       ->
                -- {{{
                Left $ "Bad arguments for a donation(s) UTxO: "
                  ++ lovelacesStr
                  ++ " "
                  ++ donCountStr
                  ++ " "
                  ++ datumStr
                -- }}}
            -- }}}
        eith = infArgHelper3 go (Right (0, 0, 0)) restOfArgs
      in
      case eith of
        Right ((inputLovelaces, inputDonations, inputWs), ocfn) ->
          -- {{{
          actOnCurrentDatum @QVFRedeemer ocfn FoldDonations Nothing $ \currDatum ->
            let
              updatedDatumFile :: FilePath
              updatedDatumFile = getFileName ocfn ocfnUpdatedDatum
              mUpdatedDatum    :: Maybe QVFDatum
              mUpdatedDatum    =
                -- {{{
                let
                  fromTotal tot =
                    -- {{{
                    if inputDonations == tot then
                      Just $ PrizeWeight (inputWs * inputWs) False
                    else
                      Just $ PrizeWeightAccumulation tot inputDonations inputWs
                    -- }}}
                in
                case currDatum of
                  ReceivedDonationsCount tot          ->
                    -- {{{
                    fromTotal tot
                    -- }}}
                  DonationFoldingProgress tot _       ->
                    -- {{{
                    fromTotal tot
                    -- }}}
                  PrizeWeightAccumulation tot done ws ->
                    -- {{{
                    let
                      newDone = done + inputDonations
                      newWs   = ws + inputWs
                    in
                    if newDone == tot then
                      Just $ PrizeWeight (newWs * newWs) False
                    else
                      Just $ PrizeWeightAccumulation tot newDone newWs
                    -- }}}
                  _                                   ->
                    -- {{{
                    Nothing
                    -- }}}
                -- }}}
              jsonToPrint      =
                -- {{{
                   "{\"lovelace\":\"" ++ show inputLovelaces
                ++ "\",\"mint\":\""   ++ show (negate inputDonations)
                ++ "\"}"
                -- }}}
            in
            case mUpdatedDatum of
              Just ud -> do
                -- {{{
                writeJSON updatedDatumFile ud
                putStrLn jsonToPrint
                -- }}}
              Nothing ->
                -- {{{
                putStrLn "FAILED: Invalid current datum."
                -- }}}
          -- }}}
        Left errMsg                                             ->
          -- {{{
          putStrLn $ "FAILED: " ++ errMsg
          -- }}}
      -- }}}
    "traverse-donations"     : loveStr0 : datStr0 : loveStr1 : datStr1 : fileNamesJSON : _     ->
      -- {{{
      case (readMaybe loveStr0, readDatum datStr0, readMaybe loveStr1, readDatum datStr1, A.decode (fromString fileNamesJSON)) of
        (Just l0, Just (Donations map0), Just l1, Just (Donations map1), Just ocfn) ->
          -- {{{
          let
            pairs0             = Map.toList map0
            go :: [(Ledger.PubKeyHash, Integer)]
               -> ( Map Ledger.PubKeyHash Integer
                  , Map Ledger.PubKeyHash Integer
                  , Integer
                  )
               -> ( Map Ledger.PubKeyHash Integer
                  , Map Ledger.PubKeyHash Integer
                  , Integer
                  )
            go lst acc         =
              -- {{{
              case (lst, acc) of
                ([], _)                     ->
                  -- {{{
                  acc
                  -- }}}
                ((k, _) : ps, (m0, m1, ls)) ->
                  -- {{{
                  -- The resulting product is:
                  --   - `m0` is the updated map for the traversing
                  --     UTxO,
                  --   - `m1` is the updated map for the traversed
                  --     UTxO,
                  --   - `ls` is the number of Lovelaces to be
                  --     transferred,
                  case Map.lookup k m1 of
                    Just lovelaces ->
                      go
                        ps
                        ( Map.unionWith (+) m0 $ Map.singleton k lovelaces
                        , Map.delete k m1
                        , ls + lovelaces
                        )
                    Nothing        ->
                      go ps (m0, m1, ls)
                  -- }}}
              -- }}}
            (m0', m1', ls1to0) = go pairs0 (map0, map1, 0)
          in
          if map0 == m0' && map1 == m1' then
            putStrLn "Nothing"
          else
            let
              updatedDatumFile :: FilePath
              updatedDatumFile = getFileName ocfn ocfnUpdatedDatum
              newDatumFile     :: FilePath
              newDatumFile     = getFileName ocfn ocfnNewDatum
              updatedDatum     = Donations m0'
              newDatum         = Donations m1'
            in do
            writeJSON updatedDatumFile updatedDatum
            writeJSON newDatumFile newDatum
            putStrLn $
                 "{\"lovelace0\":"
              ++ "\"" ++ show (l0 + ls1to0) ++ "\""
              ++ ",\"lovelace1\":"
              ++ "\"" ++ show (l1 - ls1to0) ++ "\""
              ++ "}"
          -- }}}
        _                                          ->
          -- {{{
          putStrLn $ "FAILED: Bad arguments: "
            ++ loveStr0
            ++ " "
            ++ datStr0
            ++ " "
            ++ loveStr1
            ++ " "
            ++ datStr1
            ++ " "
            ++ fileNamesJSON
          -- }}}
      -- }}}
    "accumulate-donations"   : restOfArgs                                                      ->
      -- {{{
      let
        go :: String
           -> String
           -> String
           -> (Integer, Integer, Integer, [(BuiltinByteString, QVFDatum)])
           -> Either String (Integer, Integer, Integer, [(BuiltinByteString, QVFDatum)])
        go tnStr lovelacesStr datumStr (ls, ps, ws, ds) =
          -- {{{
          let
            mTN        :: Maybe BuiltinByteString
            mTN        =
              toBuiltin . LBS.toStrict <$> hexStringToByteString tnStr
            mLovelaces :: Maybe Integer
            mLovelaces = readMaybe lovelacesStr
            mDatum     :: Maybe QVFDatum
            mDatum     = readDatum datumStr
          in
          case (mTN, mLovelaces, mDatum) of
            (Just projID, Just lovelaces, Just (PrizeWeight w False)) ->
              -- {{{
              Right
                ( lovelaces + ls - halfOfTheRegistrationFee
                , ps + 1
                , ws + w
                , ds ++ [(projID, PrizeWeight w True)]
                )
              -- }}}
            _                                                         ->
              -- {{{
              Left $ "Bad arguments:\n"
                ++ tnStr ++ " " ++ lovelacesStr ++ " " ++ datumStr
              -- }}}
          -- }}}
        eith = infArgHelper3 go (Right (0, 0, 0, [])) restOfArgs
      in
      case eith of
        Right ((inputLs, inputPs, inputWs, ds), ocfn) ->
          -- {{{
          actOnCurrentDatum @QVFRedeemer ocfn AccumulateDonations Nothing $ \currDatum ->
            let
              updatedDatumFile  :: FilePath
              updatedDatumFile  = getFileName ocfn ocfnUpdatedDatum

              mkProjDatumFile   :: BuiltinByteString -> FilePath
              mkProjDatumFile   = getProjectsDatumFile ocfn . TokenName

              eithUpdatedDatum  :: Either String QVFDatum
              eithUpdatedDatum  =
                -- {{{
                case currDatum of
                  RegisteredProjectsCount totP                                 ->
                    -- {{{
                    if inputPs == totP then
                      Right $ DonationAccumulationConcluded
                        totP inputLs inputWs False
                    else
                      Right $ DonationAccumulationProgress
                        totP
                        inputPs
                        inputLs
                        inputWs
                    -- }}}
                  DonationAccumulationProgress totP psSoFar lsSoFar sumWsSoFar ->
                    -- {{{
                    if inputPs + psSoFar == totP then
                      Right $ DonationAccumulationConcluded
                        totP (lsSoFar + inputLs) (sumWsSoFar + inputWs) False
                    else
                      Right $ DonationAccumulationProgress
                        totP
                        (inputPs + psSoFar)
                        (inputLs + lsSoFar)
                        (inputWs + sumWsSoFar)
                    -- }}}
                  _                                                            ->
                    -- {{{
                    Left "Bad project UTxO provided."
                    -- }}}
                -- }}}
            in
            case eithUpdatedDatum of
              Right updatedDatum -> do
                -- {{{
                writeJSON updatedDatumFile updatedDatum
                forM_ ds $ \(projID, projDatum) ->
                  writeJSON (mkProjDatumFile projID) projDatum
                print $ inputLs + governanceLovelaces
                -- }}}
              Left err           ->
                -- {{{
                putStrLn $ "FAILED: " ++ err
                -- }}}
          -- }}}
        Left errMsg                                      ->
          -- {{{
          putStrLn $ "FAILED: " ++ errMsg
          -- }}}
      -- }}}
    "collect-key-holder-fee" : fileNamesJSON : _                                               ->
      -- {{{
      case A.decode $ fromString fileNamesJSON of
        Just ocfn ->
          -- {{{
          actOnCurrentDatum @QVFRedeemer ocfn PayKeyHolderFee Nothing $ \case
            DonationAccumulationConcluded ps totalLovelaces sumP False ->
              -- {{{
              let
                (khFee, updatedDatum) =
                  OC.findDatumAfterPayingKeyHoldersFee ps totalLovelaces sumP

                updatedDatumFile      :: FilePath
                updatedDatumFile      = getFileName ocfn ocfnUpdatedDatum
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
          putStrLn $
               "FAILED because of bad off-chain filenames JSON:\n"
            ++ fileNamesJSON
          -- }}}
      -- }}}
    "distribute-prize"       : infoJSONStr : prizeWeightJSONStr : fileNamesJSON : _            ->
      -- {{{
      fromDatumValue (fromString infoJSONStr) $ \case
        ProjectInfo (ProjectDetails {..}) ->
          -- {{{
          fromDatumValue (fromString prizeWeightJSONStr) $ \case
            PrizeWeight w True ->
              -- {{{
              case A.decode $ fromString fileNamesJSON of
                Just ocfn ->
                  -- {{{
                  actOnCurrentDatum @QVFRedeemer ocfn DistributePrizes Nothing $ \case
                    DonationAccumulationConcluded remPs totLs den True ->
                      -- {{{
                      if remPs > 0 then
                        -- {{{
                        let
                          portion          :: Integer
                          portion          =
                            OC.findProjectsWonLovelaces totLs den w

                          prize            :: Integer
                          prize            = min pdRequested portion

                          escrowLovelaces  :: Integer
                          escrowLovelaces  =
                            OC.findEscrowLovelaces portion pdRequested

                          updatedDatumFile :: FilePath
                          updatedDatumFile = getFileName ocfn ocfnUpdatedDatum

                          updatedDatum     :: QVFDatum
                          updatedDatum     =
                            DonationAccumulationConcluded
                              (remPs - 1)
                              totLs
                              den
                              True

                          newDatumFile     :: FilePath
                          newDatumFile     = getFileName ocfn ocfnNewDatum

                          newDatum         :: QVFDatum
                          newDatum         = Escrow Map.empty
                        in do
                        writeJSON updatedDatumFile updatedDatum
                        writeJSON newDatumFile newDatum
                        putStrLn $
                             "{\"owner\":\""  ++ show prize           ++ "\""
                          ++ ",\"escrow\":\"" ++ show escrowLovelaces ++ "\""
                          ++ "}"
                        -- }}}
                      else
                        -- {{{
                        putStrLn "FAILED: All prizes are distributed."
                        -- }}}
                      -- }}}
                    _                                                  ->
                      -- {{{
                      putStrLn "FAILED: Invalid governance datum for distribution."
                      -- }}}
                  -- }}}
                Nothing   ->
                  -- {{{
                  putStrLn $
                       "FAILED because of bad off-chain filenames JSON:\n"
                    ++ fileNamesJSON
                  -- }}}
              -- }}}
            _                   ->
              -- {{{
              putStrLn "FAILED to parse the PrizeWeight datum."
              -- }}}
          -- }}}
        _                                 ->
          -- {{{
          putStrLn "FAILED to parse the ProjectInfo datum."
          -- }}}
      -- }}}
    "unlock-bounty-for"      : _                                                               ->
      putStrLn "TODO."
    "withdraw-bounty"        : _                                                               ->
      putStrLn "TODO."
    "pretty-datum"       : datumJSONStr : _                             ->
      -- {{{
      fromDatumValue (fromString datumJSONStr) print
      -- }}}
    "datum-is"           : predicateKeyWord : datumJSONStr : _          ->
      -- {{{
      case predicateKeyWordToPredicate predicateKeyWord of
        Just p  ->
          -- {{{
          fromDatumValue (fromString datumJSONStr) (print . p)
          -- }}}
        Nothing ->
          -- {{{
          print $
            "FAILED: Unsupported datum predicate (" ++ predicateKeyWord ++ ")."
          -- }}}
      -- }}}
    "get-constr-index"   : datumKeyWord : _                             ->
      -- {{{
      case readQVFDatum datumKeyWord of
        Just d  ->
          -- {{{
          case PlutusTx.builtinDataToData (PlutusTx.toBuiltinData d) of
            Constr n _ -> print n
            _          -> putStrLn "FAILED: Not a constructor."
          -- }}}
        Nothing ->
          -- {{{
          putStrLn "FAILED: Could not read the provided datum."
          -- }}}
      -- }}}
    "data-to-cbor"       : dataJSONStr : _                              ->
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
    "cbor-to-data"       : cborStr : _                                  ->
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
    "string-to-hex"      : tn : outFile : _                             ->
      -- {{{
      andPrintSuccess outFile $ writeTokenNameHex outFile $ fromString tn
      -- }}}
    "get-deadline-slot"  : currSlotStr : datumJSON : _                  -> do
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
