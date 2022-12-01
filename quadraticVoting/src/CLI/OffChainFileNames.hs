{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}


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


import Data.Aeson   ( FromJSON
                    , ToJSON )
import GHC.Generics ( Generic )


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
  } deriving (Show, Generic, ToJSON, FromJSON)


getFileName :: (OffChainFileNames -> String) -> OffChainFileNames -> FilePath
getFileName handle ocfn =
  ocfnPreDir ocfn ++ "/" ++ handle ocfn


projectsDatumFile :: String -> OffChainFileNames -> FilePath
projectsDatumFile tnStr ocfn =
  ocfnPreDir ocfn ++ "/" ++ ocfnProjectsPreDir ocfn ++ "/" ++ tnStr ++ ".datum"


projectsInfoFile :: String -> OffChainFileNames -> FilePath
projectsInfoFile tnStr ocfn = ocfnPreDir ocfn ++ "/" ++ tnStr


governanceMinter   :: OffChainFileNames -> FilePath
governanceMinter   = getFileName ocfnGovernanceMinter

registrationMinter :: OffChainFileNames -> FilePath
registrationMinter = getFileName ocfnRegistrationMinter

donationMinter     :: OffChainFileNames -> FilePath
donationMinter     = getFileName ocfnDonationMinter

qvfMainValidator   :: OffChainFileNames -> FilePath
qvfMainValidator   = getFileName ocfnQVFMainValidator

deadlineSlot       :: OffChainFileNames -> FilePath
deadlineSlot       = getFileName ocfnDeadlineSlot

deadlineDatum      :: OffChainFileNames -> FilePath
deadlineDatum      = getFileName ocfnDeadlineDatum

initialGovDatum    :: OffChainFileNames -> FilePath
initialGovDatum    = getFileName ocfnInitialGovDatum

currentDatum       :: OffChainFileNames -> FilePath
currentDatum       = getFileName ocfnCurrentDatum

updatedDatum       :: OffChainFileNames -> FilePath
updatedDatum       = getFileName ocfnUpdatedDatum

newDatum           :: OffChainFileNames -> FilePath
newDatum           = getFileName ocfnNewDatum

qvfRedeemer        :: OffChainFileNames -> FilePath
qvfRedeemer        = getFileName ocfnQVFRedeemer

minterRedeemer     :: OffChainFileNames -> FilePath
minterRedeemer     = getFileName ocfnMinterRedeemer

projectTokenName   :: OffChainFileNames -> FilePath
projectTokenName   = getFileName ocfnProjectTokenName

