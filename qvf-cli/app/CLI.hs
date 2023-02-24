{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE LambdaCase        #-}


module Main (main) where


-- IMPORTS
-- {{{
import Debug.Trace (trace)

import           Cardano.Api
import qualified Data.Aeson                 as A
import           Data.Aeson                 ( encode )
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.List                  as List
import           Data.List                  ( sortBy )
import           Data.Maybe                 ( fromJust )
import           Data.String                ( fromString )
import qualified Data.Text                  as T
import qualified Ledger.Address             as Addr
import           Plutus.V1.Ledger.Value     ( TokenName(..) )
import qualified Plutus.V2.Ledger.Api       as Ledger
import           PlutusTx                   ( Data (..) )
import qualified PlutusTx
import qualified PlutusTx.AssocMap          as Map
import           PlutusTx.AssocMap          ( Map )
import           System.Environment         ( getArgs )
import           Text.Read                  ( readMaybe )
--
import           Data.Datum
import           Data.DonationInfo
import           Data.Redeemer
--
import qualified CLI.Help                   as Help
import qualified CLI.OffChainFileNames      as OCFN
import qualified CLI.RegisteredProject      as RP
import           CLI.Utils
import qualified CLI.Tx                     as Tx
import           CLI.Tx                     ( Asset(..)
                                            , Input(..)
                                            , Output(..)
                                            , txOutToOutput
                                            , ScriptInput(..)
                                            , ScriptOutput(..) )
import qualified QVF                        as OC
import qualified Minter.Donation            as Don
import qualified Minter.Governance          as Gov
import qualified Minter.Registration        as Reg
import           Utils
-- }}}


-- APPLICATION
-- {{{
main :: IO ()
main =
  let
    inputsRefsOutputsJSONHelper  :: ([Input], [Input], [Output])
                                 -> Maybe String
                                 -> String
    inputsRefsOutputsJSONHelper (ins, refs, outs) mX   =
      -- {{{
         "{\"inputs\":"  ++ show ins
      ++ ",\"refs\":"    ++ show refs
      ++ ",\"outputs\":" ++ show outs
      ++ ( case mX of
             Just misc -> ",\"extra\":" ++ misc
             Nothing   -> ""
         )
      ++ "}"
      -- }}}
    inputsRefsOutputsJSON :: String
                          -> ([Input], [Input], [Ledger.TxOut])
                          -> String
    inputsRefsOutputsJSON scriptAddr (ins, refs, outs) =
      -- {{{
      case traverse (txOutToOutput scriptAddr) outs of
        Just scriptOuts ->
          -- {{{
          inputsRefsOutputsJSONHelper (ins, refs, scriptOuts) Nothing
          -- }}}
        Nothing         ->
          -- {{{
          trace (show outs) "FAILED: Couldn't convert `TxOut` values to `Output` values."
          -- }}}
      -- }}}
    sortInputsRefsBy :: (Int -> QVFDatum -> Bool)
                     -> [Input]
                     -> [Input]
                     -> ([Input], [Input])
    sortInputsRefsBy predicate projs refs              =
      -- {{{
      let
        sortFn                                = sortBy Tx.compareInputs
        go :: [Input]
           -> [Input]
           -> ([Input], [Input])
           -> ([Input], [Input])
        go (p : ps) (i : is) acc@(pAcc, iAcc) =
          -- {{{
          let
            cond =
              predicate
                (length pAcc)
                (getInlineDatum $ Tx.outputToTxOut $ iResolved p)
          in
          if cond then
            go ps is (p : pAcc, i : iAcc)
          else
            go ps is acc
          -- }}}
        go _        _        acc              = acc
      in
      go (sortFn projs) (sortFn refs) ([], [])
      -- }}}
    emulateFromInputs :: [Input] -> Either String [Tx.DistributionInfo]
    emulateFromInputs allInputs                        =
      -- {{{
      let
        mSeparated :: Maybe
                        ( [Tx.ProjectStateInput]
                        , [Tx.ProjectInfoInput]
                        , [Tx.DonationInput]
                        , Tx.MatchPoolInput
                        )
        mSeparated = do
          -- {{{
          allSIs <- traverse Tx.inputToScriptInput allInputs
          let foldFn si acc@(states, infos, dons, mMPO) =
                -- {{{
                let
                  ref = siTxOutRef si
                  so  = siResolved si
                  ls  = soLovelace so
                  sym = assetSymbol    $ soAsset so
                  tn  = assetTokenName $ soAsset so
                in
                case soDatum so of
                  RegisteredProjectsCount _ ->
                    ( states
                    , infos
                    , dons
                    , Just $ Tx.MatchPoolInput
                        { mpiTxOutRef = ref
                        , mpiAddress  = soAddress so
                        , mpiLovelace = soLovelace so - governanceLovelaces
                        , mpiSymbol   = sym
                        }
                    )
                  ReceivedDonationsCount _  ->
                    ( Tx.ProjectStateInput ref ls sym tn : states
                    , infos
                    , dons
                    , mMPO
                    )
                  ProjectInfo ds            ->
                    ( states
                    , Tx.ProjectInfoInput ref ls sym tn ds : infos
                    , dons
                    , mMPO
                    )
                  Donation donor            ->
                    ( states
                    , infos
                    , Tx.DonationInput ref ls sym tn donor : dons
                    , mMPO
                    )
                  _                         ->
                    acc
                -- }}}
              (initStates, initInfos, initDons, mGov)   =
                foldr foldFn ([], [], [], Nothing) allSIs
          matchPoolInput <- mGov
          return
            ( sortBy
                ( \s0 s1 ->
                    compare (Tx.psiTokenName s0) (Tx.psiTokenName s1)
                )
                initStates
            , sortBy
                ( \i0 i1 ->
                    compare (Tx.piiTokenName i0) (Tx.piiTokenName i1)
                )
                initInfos
            , sortBy
                ( \d0 d1 ->
                    compare (Tx.diTokenName d0) (Tx.diTokenName d1)
                )
                initDons
            , matchPoolInput
            )
          -- }}}
      in
      case mSeparated of
        Just (pStates, pInfos, allDons, mpi) ->
          -- {{{
          let
            -- Note that the `Tx.MatchPoolInput` value was constructed such
            -- that its Lovelace count directly represented the match pool
            -- amount (i.e. excluding the initial governance Lovelaces).
            matchPool                 = Tx.mpiLovelace mpi
            go :: [Tx.ProjectStateInput]
               -> [Tx.ProjectInfoInput]
               -> Maybe (Map Ledger.BuiltinByteString EliminationInfo)
               -> Maybe (Map Ledger.BuiltinByteString EliminationInfo)
            go (p : ps) (i : is) mAcc = do
              -- {{{
              elimMap <- mAcc
              let tn@(TokenName tnBS)  = Tx.psiTokenName p
                  dons                 =
                    filter (\di -> Tx.diTokenName di == tn) allDons
                  foldFn d (tot, dMap) =
                    -- {{{
                    ( tot + Tx.diLovelace d
                    , Map.unionWith
                        (+)
                        (Map.singleton (Tx.diDonor d) (Tx.diLovelace d))
                        dMap
                    )
                    -- }}}
                  (totDs, donMap)      = foldr foldFn (0, Map.empty) dons
                  prizeWeight          = Don.foldDonationsMap donMap
                  eliminationInfo      =
                    -- {{{
                    EliminationInfo
                      { eiRequested = pdRequested $ Tx.piiDetails i
                      , eiRaised    = totDs
                      , eiWeight    = prizeWeight
                      }
                    -- }}}
              go ps is $ Just $ Map.insert tnBS eliminationInfo elimMap
              -- }}}
            go []       []       mAcc = mAcc
            go _        _        _    = Nothing
            mElimMap                  = go pStates pInfos (Just Map.empty)
            eliminateAll :: Map Ledger.BuiltinByteString EliminationInfo
                         -> [Tx.DistributionInfo]
                         -> ([Tx.DistributionInfo], [Tx.DistributionInfo])
            eliminateAll eMap elims   =
              -- {{{
              let
                kvs          = Map.toList eMap
                den          = findQVFDenominator kvs
                (_, tnBS, r) = findLeastFundedProject matchPool den kvs
                toDistrInfo  =
                  Tx.eliminationInfoToDistributionInfo matchPool den
              in
              if r < decimalMultiplier then
                -- WARNING: Take caution. Here we've carefully used
                -- `fromJust` to prevent further bloating of the on-chain
                -- code, and further complexity for this part of the
                -- emulation.
                eliminateAll
                  (Map.delete tnBS eMap)
                  ( let
                      elimInfo  = fromJust $ Map.lookup tnBS eMap
                    in
                    toDistrInfo (tnBS, elimInfo) : elims
                  )
              else
                (toDistrInfo <$> Map.toList eMap, elims)
              -- }}}
          in
          case mElimMap of
            Just elimMap ->
              -- {{{
              let
                (eligs, elims) = eliminateAll elimMap []
              in
              Right $ eligs ++ elims
              -- }}}
            Nothing      ->
              -- {{{
              Left "Couldn't find the elimination map."
              -- }}}
          -- }}}
        Nothing                              ->
          -- {{{
          Left "Provided inputs don't present a valid state for a running funding round."
          -- }}}
      -- }}}
    handleOneProject :: [String]
                     -> (    (Input, Asset)
                          -> Input
                          -> (Input, Asset)
                          -> OCFN.OffChainFileNames
                          -> IO ()
                        )
                     -> IO ()
    handleOneProject args handler                      =
      -- {{{
      case args of
        govInputStr : infoInputStr : projInputStr : ocfnStr : _ ->
          -- {{{
          let
            mGov  = decodeString @Input govInputStr
            mInfo = decodeString @Input infoInputStr
            mProj = decodeString @Input projInputStr
            mA    = Tx.getAssetFromInput
          in
          case (mGov, mInfo, mProj, decodeString ocfnStr) of
            (Just govInput, Just infoInput, Just projInput, Just ocfn) ->
              -- {{{
              case (mA govInput, mA projInput) of
                (Just govAsset, Just projAsset) ->
                  -- {{{
                  handler
                    (govInput, govAsset)
                    infoInput
                    (projInput, projAsset)
                    ocfn
                  -- }}}
                _                               ->
                  -- {{{
                  putStrLn "FAILED: Bad UTxOs provided."
                  -- }}}
              -- }}}
            _                                                          ->
              -- {{{
              putStrLn $ "FAILED: Bad arguments:"
                ++ "\n\t" ++ govInputStr
                ++ "\n\t" ++ infoInputStr
                ++ "\n\t" ++ projInputStr
                ++ "\n\t" ++ ocfnStr
              -- }}}
          -- }}}
        _                                                       ->
          -- {{{
          putStrLn "FAILED: Not enough arguments provided."
          -- }}}
      -- }}}
  in do
  allArgs <- getArgs
  case allArgs of
    -- {{{ HELP, MAN, AND VERSION ENDPOINTS 
    []                                 -> putStrLn Help.generic
    "-h"       : _                     -> putStrLn Help.generic
    "--help"   : _                     -> putStrLn Help.generic
    "man"      : _                     -> putStrLn Help.generic
    "-v"        : _                    -> putStrLn "0.2.5.2"
    "--version" : _                    -> putStrLn "0.2.5.2" 
    "generate" : genStr : "-h"     : _ -> putStrLn $ Help.forGenerating genStr
    "generate" : genStr : "--help" : _ -> putStrLn $ Help.forGenerating genStr
    "generate" : genStr : "man"    : _ -> putStrLn $ Help.forGenerating genStr
    actionStr  : "-h"     : _          -> putStrLn $ Help.forEndpoint actionStr
    actionStr  : "--help" : _          -> putStrLn $ Help.forEndpoint actionStr
    actionStr  : "man"    : _          -> putStrLn $ Help.forEndpoint actionStr
    -- }}}
    -- {{{ SMART CONTRACT INTERACTION ENDPOINTS 
    "generate" : "scripts" : pkhStr : txRefStr : currSlotStr : deadlineStr : fileNamesJSON : _                           ->
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
              govOF     = OCFN.governanceMinter   ocfn
              regOF     = OCFN.registrationMinter ocfn
              donOF     = OCFN.donationMinter     ocfn
              qvfOF     = OCFN.qvfMainValidator   ocfn
              dlDatOF   = OCFN.deadlineDatum      ocfn
              initDatOF = OCFN.initialGovDatum    ocfn
              dlSlotOF  = OCFN.deadlineSlot       ocfn
              redOF     = OCFN.minterRedeemer     ocfn
              --
              govPolicy = Gov.qvfPolicy pkh txRef dl
              govSymbol = mintingPolicyToSymbol govPolicy
              --
              regPolicy = Reg.registrationPolicy pkh govSymbol
              regSymbol = mintingPolicyToSymbol regPolicy
              --
              donPolicy = Don.donationPolicy pkh regSymbol
              donSymbol = mintingPolicyToSymbol donPolicy

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
                  withSuccessMessageAndSize
                    govOF
                    (unsafeParseJSON @OutputPlutus)
                    (return ())
                  withSuccessMessageAndSize
                    regOF
                    (unsafeParseJSON @OutputPlutus)
                    (return ())
                  withSuccessMessageAndSize
                    donOF
                    (unsafeParseJSON @OutputPlutus)
                    (return ())
                  withSuccessMessageAndSize
                    qvfOF
                    (unsafeParseJSON @OutputPlutus)
                    (return ())
                  withSuccessMessage
                    dlSlotOF
                    (LBS.writeFile dlSlotOF $ encode dlSlot)
                  withSuccessMessage dlDatOF $ writeJSON dlDatOF $ deadlineDatum dl
                  withSuccessMessage initDatOF $ writeJSON initDatOF initialGovDatum
                  withSuccessMessage redOF $ writeJSON redOF Gov.Initiate
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
    "update-deadline"             : currSlotStr : deadlineStr : fileNamesJSON : _                                        ->
      -- {{{
      case (readMaybe currSlotStr, Ledger.POSIXTime <$> readMaybe deadlineStr, decodeString fileNamesJSON) of
        (Just currSlot, Just newDL, Just ocfn) ->
          -- {{{
          let
            qvfDatumFile    = OCFN.deadlineDatum ocfn
            qvfDatum        = DeadlineDatum newDL
            qvfRedeemerFile = OCFN.qvfRedeemer ocfn
            qvfRedeemer     = UpdateDeadline newDL
            dlSlotFile      = OCFN.deadlineSlot ocfn
          in do
          dlSlot <- getDeadlineSlot currSlot dl
          withSuccessMessage dlSlotFile
            (LBS.writeFile dlSlotFile $ encode dlSlot)
          withSuccessMessage qvfDatumFile $
            writeJSON qvfDatumFile qvfDatum
          withSuccessMessage qvfRedeemerFile $
            writeJSON qvfRedeemerFile qvfRedeemer
          -- }}}
        _                                      ->
          -- {{{
          putStrLn $ "FAILED with bad arguments:"
            ++ "\n\t" ++ currSlotStr
            ++ "\n\t" ++ deadlineStr
            ++ "\n\t" ++ fileNamesJSON
          -- }}}
      -- }}}
    "register-project"            : pkhStr : lbl : reqFundStr : fileNamesJSON : _                                        ->
      -- {{{
      case (readMaybe reqFundStr, decodeString fileNamesJSON) of
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
                updatedDatumFile  = OCFN.updatedDatum ocfn

                newDatum          :: QVFDatum
                newDatum          = ReceivedDonationsCount 0
                newDatumFile      :: FilePath
                newDatumFile      = OCFN.newDatum ocfn

                projTokenName     :: TokenName
                projTokenName     = indexToTokenName soFar
                projTokenNameFile :: FilePath
                projTokenNameFile = OCFN.projectTokenName ocfn

                projInfo          :: QVFDatum
                projInfo          = ProjectInfo projDetails
                projInfoFile      :: FilePath
                projInfoFile      =
                  OCFN.projectsInfoFile
                    (unsafeTokenNameToHex projTokenName)
                    ocfn
              in do
              withSuccessMessage projTokenNameFile $
                writeTokenNameHex projTokenNameFile projTokenName
              withSuccessMessage updatedDatumFile $
                writeJSON updatedDatumFile updatedDatum
              withSuccessMessage newDatumFile $
                writeJSON newDatumFile newDatum
              withSuccessMessage projInfoFile $
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
    "donate-to-project"           : pkhStr : projIDStr : amtStr : fileNamesJSON : _                                      ->
      -- {{{
      case (hexStringToBuiltinByteString projIDStr, readMaybe amtStr, decodeString fileNamesJSON) of
        (Just projID, Just amt, Just ocfn) ->
          -- {{{
          let
            govRedeemer :: QVFRedeemer
            govRedeemer = DonateToProject

            donorPKH    :: Ledger.PubKeyHash
            donorPKH    = fromString pkhStr

            donInfo     :: DonationInfo
            donInfo     = DonationInfo projID donorPKH amt

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
                updatedDatumFile = OCFN.updatedDatum ocfn

                newDatum         :: QVFDatum
                newDatum         = Donation donorPKH
                newDatumFile     :: FilePath
                newDatumFile     = OCFN.newDatum ocfn
              in do
              withSuccessMessage updatedDatumFile $
                writeJSON updatedDatumFile updatedDatum
              withSuccessMessage newDatumFile $
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
    "contribute"                  : amtStr : fileNamesJSON : _                                                           ->
      -- {{{
      case (readMaybe amtStr, decodeString fileNamesJSON) of
        (Just amt, Just ocfn) ->
          -- {{{
          let
            qvfRedeemerFile = OCFN.qvfRedeemer ocfn
            qvfRedeemer     = Contribute amt
          in
          withSuccessMessage qvfRedeemerFile $
            writeJSON qvfRedeemerFile qvfRedeemer
          -- }}}
        _                     ->
          -- {{{
          putStrLn $ "FAILED with bad arguments:"
            ++ "\n\t" ++ amtStr
            ++ "\n\t" ++ fileNamesJSON
          -- }}}
      -- }}}
    "fold-donations"              : restOfArgs                                                                           ->
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
              updatedDatumFile    = OCFN.updatedDatum ocfn
              newDatumFile        :: FilePath
              newDatumFile        = OCFN.newDatum ocfn
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
                putStrLn $
                     "FAILED: Bad project UTxO provided ("
                  ++ show currDatum
                  ++ ")."
                -- }}}
          -- }}}
        Left errMsg                                               ->
          -- {{{
          putStrLn $ "FAILED: " ++ errMsg
          -- }}}
      -- }}}
    "consolidate-donations"       : restOfArgs                                                                           ->
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
              _                                              ->
                -- {{{
                Left $ "Bad arguments for a donation(s) UTxO:"
                  ++ "\n\t" ++ lovelacesStr
                  ++ "\n\t" ++ donCountStr
                  ++ "\n\t" ++ datumStr
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
              updatedDatumFile = OCFN.updatedDatum ocfn
              mUpdatedDatum    :: Maybe QVFDatum
              mUpdatedDatum    =
                -- {{{
                let
                  fromRemainingAndPrevWs remaining prevWs =
                    -- {{{
                    let
                      finalWs = prevWs + inputWs
                    in
                    if inputDonations == remaining then
                      Just $ PrizeWeight (finalWs * finalWs) False
                    else
                      Just $ ConsolidationProgress (remaining - inputDonations) finalWs
                    -- }}}
                in
                case currDatum of
                  ReceivedDonationsCount tot         ->
                    -- {{{
                    fromRemainingAndPrevWs tot 0
                    -- }}}
                  DonationFoldingProgress tot folded ->
                    -- {{{
                    if tot == folded then
                      fromRemainingAndPrevWs tot 0
                    else
                      Nothing
                    -- }}}
                  ConsolidationProgress remaining ws ->
                    -- {{{
                    fromRemainingAndPrevWs remaining ws
                    -- }}}
                  _                                  ->
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
    "traverse-donations"          : loveStr0 : datStr0 : loveStr1 : datStr1 : fileNamesJSON : _                          ->
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
              updatedDatumFile = OCFN.updatedDatum ocfn
              newDatumFile     :: FilePath
              newDatumFile     = OCFN.newDatum ocfn
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
    "accumulate-prize-weights"    : inputCntStr : govInputStr : infoInputsStr : projInputsStr : fileNamesJSON : _        ->
      -- {{{
      Tx.fromGovAndInputs govInputStr projInputsStr (Just infoInputsStr) $
        \govInput@Input{iResolved = govO} projInputs infoInputs ->
          -- {{{
          case projInputs of
            p0 : _ ->
              -- {{{
              case (readMaybe inputCntStr, Tx.getAssetFromInput govInput, Tx.getAssetFromInput p0, decodeString fileNamesJSON) of
                (Just inputCount, Just govAsset, Just pAsset, Just ocfn) ->
                  -- {{{
                  let
                    scriptAddr                = oAddressStr govO
                    predicate accCount qvfD   =
                      -- {{{
                         (accCount < inputCount)
                      && ( case qvfD of
                             PrizeWeight _ False ->
                               True
                             _                   ->
                               False
                         )
                      -- }}}
                    (finalProjs', finalInfos) =
                      sortInputsRefsBy predicate projInputs infoInputs
                    finalProjs                = finalProjs' ++ [govInput]
                    outputs                   =
                      -- {{{
                      OC.accumulatePrizeWeights
                        (oAddress govO)
                        (assetSymbol govAsset)
                        (assetSymbol pAsset)
                        (Tx.inputToTxInInfo <$> finalProjs)
                        (Tx.inputToTxInInfo <$> finalInfos)
                      -- }}}
                  in do
                  writeJSON (OCFN.qvfRedeemer ocfn) AccumulatePrizeWeights
                  putStrLn $
                    inputsRefsOutputsJSON
                      scriptAddr
                      (finalProjs, finalInfos, outputs)
                  -- }}}
                _                                                        ->
                  -- {{{
                  putStrLn "FAILED: Invalid governance or project input."
                  -- }}}
              -- }}}
            _      ->
              -- {{{
              putStrLn "FAILED: No projects found."
              -- }}}
          -- }}}
      -- }}}
    "eliminate-one-project"       : govInputStr : infoInputsStr : projInputsStr : registeredProjsStr : fileNamesJSON : _ ->
      -- {{{
      Tx.fromGovAndInputs govInputStr projInputsStr (Just infoInputsStr) $
        \govInput@Input{iResolved = govO} projInputs infoInputs ->
          -- {{{
          let
            scriptAddr = oAddressStr govO
            currUTxO   = Tx.outputToTxOut govO
          in
          case projInputs of
            p0 : _ ->
              -- {{{
              case (Tx.getAssetFromInput p0, Tx.getAssetFromInput govInput, getInlineDatum currUTxO) of
                (Just pAsset, Just govAsset, ProjectEliminationProgress mp wMap) ->
                  -- {{{
                  let
                    predicate _ qvfD               =
                      -- {{{
                      case qvfD of
                        PrizeWeight _ True -> True
                        _                  -> False
                      -- }}}
                    (finalProjs, finalInfos)       =
                      sortInputsRefsBy predicate projInputs infoInputs
                    (validOutputs, mEliminated, r) =
                      OC.eliminateOneProject
                        (assetSymbol govAsset)
                        (assetSymbol pAsset)
                        currUTxO
                        (Tx.inputToTxInInfo <$> finalProjs)
                        (Tx.inputToTxInInfo <$> finalInfos)
                        mp
                        wMap
                  in
                  case mEliminated of
                    Just (pkh, raised) ->
                      -- {{{
                      let
                        mQVFOutputs :: Maybe [Output]
                        mQVFOutputs                     =
                          -- {{{
                          traverse (txOutToOutput scriptAddr) validOutputs
                          -- }}}
                        mEliminatedAsset :: Maybe Asset
                        mEliminatedAsset                =
                          -- {{{
                          let
                            foldFn :: Output -> Maybe Asset -> Maybe Asset
                            foldFn o mFound =
                              -- {{{
                              case (mFound, oForScript o) of
                                (Just _ , _             ) ->
                                  mFound
                                (Nothing, Just (a, _, _)) ->
                                  if assetSymbol a == assetSymbol pAsset then
                                    Just a
                                  else
                                    Nothing
                                _                         ->
                                  Nothing
                              -- }}}
                          in do
                          qvfOuts <- mQVFOutputs
                          foldr foldFn Nothing qvfOuts
                          -- }}}
                        projGo :: Input -> [Input] -> Maybe Input
                        projGo ref (p : ps)             =
                          -- {{{
                          case (Tx.getAssetFromInput ref, Tx.getAssetFromInput p) of
                            (Just refA, Just pA) | refA == pA ->
                              Just p
                            _                                 ->
                              projGo ref ps
                          -- }}}
                        projGo _   _                    =
                          -- {{{
                          Nothing
                          -- }}}
                        refIsProper :: Output -> Bool
                        refIsProper refO                =
                          -- {{{
                          case getInlineDatum $ Tx.outputToTxOut refO of
                            ProjectInfo ProjectDetails{..} ->
                              -- {{{
                              case (mEliminatedAsset, oForScript refO) of
                                (Just eliminatedA, Just (infoA, _, _)) ->
                                  infoA == eliminatedA && pdPubKeyHash == pkh
                                _                                      ->
                                  False
                              -- }}}
                            _                              ->
                              -- {{{
                              False
                              -- }}}
                          -- }}}
                        infoGo :: [Input]
                               -> Maybe ([Input], [Input], [Output])
                        infoGo (ref : refs)             =
                          -- {{{
                          if refIsProper $ iResolved ref then do
                            -- {{{
                            qvfOuts   <- traverse (txOutToOutput scriptAddr) validOutputs
                            proj      <- projGo ref projInputs
                            addrStr   <- decodeString registeredProjsStr >>= RP.lookupAddressWithPKH pkh
                            ownerAddr <- tryReadAddress $ T.pack addrStr
                            let outputs =
                                  if raised > 0 then
                                    let
                                      toOwner =
                                        Output
                                          ownerAddr
                                          addrStr
                                          raised
                                          Nothing
                                    in
                                    toOwner : qvfOuts
                                  else
                                    qvfOuts
                            return ([govInput, proj], [ref], outputs)
                            -- }}}
                          else
                            -- {{{
                            infoGo refs
                            -- }}}
                          -- }}}
                        infoGo _                        =
                          -- {{{
                          Nothing
                          -- }}}
                      in
                      case (infoGo infoInputs, decodeString fileNamesJSON) of
                        (Just res, Just ocfn) -> do
                          writeJSON (OCFN.qvfRedeemer ocfn) EliminateOneProject
                          putStrLn $ inputsRefsOutputsJSONHelper res $ Just $ show r
                        _                     ->
                          putStrLn "FAILED: Something went wrong."
                      -- }}}
                    Nothing            ->
                      -- {{{
                      putStrLn $
                        inputsRefsOutputsJSON
                          scriptAddr
                          ([govInput], [], validOutputs)
                      -- }}}
                  -- }}}
                _                                                                ->
                  -- {{{
                  putStrLn "FAILED: Either bad project input, bad governance value, or invalid current datum."
                  -- }}}
              -- }}}
            _      ->
              -- {{{
              putStrLn "FAILED: No projects found."
              -- }}}
          -- }}}
      -- }}}
    "distribute-prize"            : ownerAddrStr : restOfArgs                                                            ->
      -- {{{
      handleOneProject restOfArgs $
        \(govInput, govAsset) infoInput (projInput, projAsset) ocfn ->
          -- {{{
          let
            govO     = iResolved govInput
            currUTxO = Tx.outputToTxOut govO
          in
          case getInlineDatum currUTxO of
            DistributionProgress mp remaining den ->
              -- {{{
              if remaining > 0 then
                -- {{{
                let
                  (outputs, winner, won, interVals) =
                    -- {{{
                    OC.distributePrize
                      (assetSymbol govAsset)
                      (assetSymbol projAsset)
                      (assetTokenName projAsset)
                      currUTxO
                      [Tx.inputToTxInInfo projInput]
                      [Tx.inputToTxInInfo infoInput]
                      mp
                      remaining
                      den
                    -- }}}
                  scriptAddrStr          = oAddressStr govO
                  mFinalTriplet          = do
                    -- {{{
                    scriptTxOuts <- traverse (txOutToOutput scriptAddrStr) outputs
                    ownerAddr    <- tryReadAddress $ T.pack ownerAddrStr
                    ownerPKH     <- Addr.toPubKeyHash ownerAddr
                    finalOutputs <- if ownerPKH == winner then do
                                      let toOwner =
                                            Output ownerAddr ownerAddrStr won Nothing
                                      if won > 0 then
                                        return $ toOwner : scriptTxOuts
                                      else
                                        return scriptTxOuts
                                    else
                                      Nothing
                    return
                      ([govInput, projInput], [infoInput], finalOutputs)
                    -- }}}
                in
                case mFinalTriplet of
                  Just res -> do
                    writeJSON
                      (OCFN.qvfRedeemer ocfn)
                      (   DistributePrize
                        $ unTokenName
                        $ assetTokenName projAsset
                      )
                    putStrLn $ inputsRefsOutputsJSONHelper res $ Just $ show interVals
                  _        ->
                    putStrLn
                      "FAILED: Couldn't convert `TxOut` values to `Output` values."
                -- }}}
              else
                -- {{{
                putStrLn "FAILED: No projects left."
                -- }}}
              -- }}}
            _                                     ->
              -- {{{
              putStrLn "FAILED: Invalid input governance datum."
              -- }}}
          -- }}}
      -- }}}
    "unlock-bounty-for"           : _                                                                                    ->
      putStrLn "TODO."
    "withdraw-bounty"             : _                                                                                    ->
      putStrLn "TODO."
    "remove-donationless-project" : restOfArgs                                                                           ->
      -- {{{
      handleOneProject restOfArgs $
        \(govInput, _) _ (projInput, projAsset) ocfn ->
          -- {{{
          let
            govO           = iResolved govInput
            projO          = iResolved projInput
            currUTxO       = Tx.outputToTxOut govO
            projUTxO       = Tx.outputToTxOut projO
            mUpdatedDatum  =
              -- {{{
              case getInlineDatum currUTxO of
                RegisteredProjectsCount ps         ->
                  Just $ RegisteredProjectsCount (ps - 1)
                PrizeWeightAccumulation ps elimMap ->
                  Just $ PrizeWeightAccumulation (ps - 1) elimMap
                _                                  ->
                  Nothing
              -- }}}
            isDonationless =
              -- {{{
              case getInlineDatum projUTxO of
                ReceivedDonationsCount 0 -> True
                _                        -> False
              -- }}}
          in
          case mUpdatedDatum of
            Just updatedDatum ->
              -- {{{
              if isDonationless then do
                -- {{{
                writeJSON (OCFN.updatedDatum ocfn) updatedDatum
                writeJSON (OCFN.qvfRedeemer ocfn)  ConcludeProject
                writeJSON
                  (OCFN.minterRedeemer ocfn)
                  (   Reg.ConcludeAndRefund
                    $ unTokenName
                    $ assetTokenName projAsset
                  )
                putStrLn "Generated redeemers and the updated datum SUCCESSFULLY."
                -- }}}
              else
                -- {{{
                putStrLn "FAILED: Provided project doesn't have the proper datum."
                -- }}}
              -- }}}
            Nothing           ->
              -- {{{
              putStrLn "FAILED: Invalid input governance datum."
              -- }}}
          -- }}}
      -- }}}
    -- }}}
    -- {{{ UTILITY ENDPOINTS 
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
        (\parseError -> putStrLn $ "FAILED to parse data JSON: " ++ parseError)
        (putStrLn . dataToCBOR)
        -- }}}
    "cbor-to-data"       : cborStr : _                                  ->
      -- {{{
      case cborStringToData cborStr of
        Just d  ->
          let
            initial =
                show
              $ encode
              $ scriptDataToJson ScriptDataJsonDetailedSchema
              $ dataToScriptData d
          in
          putStrLn $ tail $ List.init $ filter (\c -> c /= '\\') initial
        Nothing -> do
          putStrLn "FAILED to decode CBOR."
      -- }}}
    "string-to-hex"      : tn : outFile : _                             ->
      -- {{{
      withSuccessMessage outFile $ writeTokenNameHex outFile $ fromString tn
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
    "emulate-outcome"    : scriptInputsStr : _                          -> do
      -- {{{
      case decodeString @[Input] scriptInputsStr of
        Just allInputs ->
          -- {{{
          case emulateFromInputs allInputs of
            Right distInfos ->
              -- {{{
              let
                emulationResult =
                  Tx.EmulationResult distInfos (iTxOutRef <$> allInputs)
              in
              print emulationResult
              -- }}}
            Left err        ->
              -- {{{
              putStrLn $ "FAILED: " ++ err
              -- }}}
          -- }}}
        Nothing        ->
          -- {{{
          putStrLn $ "FAILED: Bad arguments:"
            ++ "\n\t" ++ scriptInputsStr
          -- }}}
      -- }}}
    "pretty-leaderboard" : scriptInputsStr : _                          -> do
      -- {{{
      case decodeString @[Input] scriptInputsStr of
        Just allInputs ->
          -- {{{
          case emulateFromInputs allInputs of
            Right distInfos ->
              -- {{{
                putStrLn
              $ List.intercalate "\n"
              $ fmap Tx.prettyDistributionInfo
              $ sortBy
                  (\di0 di1 -> compare (Tx.diRatioNum di1) (Tx.diRatioNum di0))
                  distInfos
              -- }}}
            Left err        ->
              -- {{{
              putStrLn $ "FAILED: " ++ err
              -- }}}
          -- }}}
        Nothing        ->
          -- {{{
          putStrLn $ "FAILED: Bad arguments:"
            ++ "\n\t" ++ scriptInputsStr
          -- }}}
      -- }}}
    -- }}}
    "test" : utxosStr : _                                               ->
      let
        mUTxOs :: Maybe [Input]
        mUTxOs = A.decode $ fromString utxosStr
      in
      case mUTxOs of
        Just utxos ->
          print $ iResolved <$> utxos
        Nothing    ->
          putStrLn "FAILED"
    _                                                                   ->
      putStrLn "FAILED: Invalid arguments for QVF-CLI."
-- }}}
