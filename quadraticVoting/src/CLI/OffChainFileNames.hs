module CLI.OffChainFileNames
  ( OffChainFileNames
  , projectsInfoFile
  , projectsDatumFile
  , governanceMinter
  , registrationMinter
  , donationMinter
  , qvfMainValidator
  , deadlineSlot
  , deadlineDatum
  , initialGovDatum
  , currentDatum
  , updatedDatum
  , newDatum
  , qvfRedeemer
  , minterRedeemer
  , projectTokenName
  ) where


import Plutus.V1.Ledger.Value ( TokenName(..) )


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


getFileName :: (OffChainFileNames -> String) -> OffChainFileNames -> FilePath
getFileName handle ocfn =
  ocfnPreDir ocfn ++ "/" ++ handle ocfn


projectsDatumFile :: TokenName -> OffChainFileNames -> FilePath
projectsDatumFile tn ocfn =
     ocfnPreDir ocfn
  ++ "/" ++ ocfnProjectsPreDir ocfn
  ++ "/" ++ unsafeTokenNameToHex tn ++ ".datum"


projectsInfoFile :: TokenName -> OffChainFileNames -> FilePath
projectsInfoFile tn ocfn =
  ocfnPreDir ocfn ++ "/" ++ unsafeTokenNameToHex tn


governanceMinter   = getFileName ocfnGovernanceMinter
registrationMinter = getFileName ocfnRegistrationMinter
donationMinter     = getFileName ocfnDonationMinter
qvfMainValidator   = getFileName ocfnQVFMainValidator
deadlineSlot       = getFileName ocfnDeadlineSlot
deadlineDatum      = getFileName ocfnDeadlineDatum
initialGovDatum    = getFileName ocfnInitialGovDatum
currentDatum       = getFileName ocfnCurrentDatum
updatedDatum       = getFileName ocfnUpdatedDatum
newDatum           = getFileName ocfnNewDatum
qvfRedeemer        = getFileName ocfnQVFRedeemer
minterRedeemer     = getFileName ocfnMinterRedeemer
projectTokenName   = getFileName ocfnProjectTokenName
