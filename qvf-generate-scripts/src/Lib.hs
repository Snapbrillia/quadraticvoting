{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib where

import           GHC.Generics         ( Generic )
import           Data.Aeson           ( FromJSON, ToJSON )
import           Aws.Lambda           ( Context )
import           Ledger               ( PubKeyHash
                                      , POSIXTime
                                      , TxOutRef
                                      , TokenName )
import qualified Ledger.Typed.Scripts as Scripts

import           Datum                ( QVFDatum(RegisteredProjectsCount) )

import qualified QVF                  as OC
import qualified Minter.Donation      as Don
import qualified Minter.Governance    as Gov
import qualified Minter.Registration  as Reg

data GenerateScriptsParams = GenerateScriptsParams
  { keyHolderPubKeyHash :: PubKeyHash
  , txRef               :: TxOutRef
  , authTokenName       :: TokenName
  , deadline            :: POSIXTime
  } 
  deriving (Generic, FromJSON, ToJSON)

data GenerateScriptsResponse = GenerateScriptsResponse
  { validator     :: Scripts.Validator
  , mintingPolicy :: Scripts.MintingPolicy
  , unitRedeemer  :: ()
  , initialDatum  :: QVFDatum
  } 
  deriving (Generic, FromJSON, ToJSON)

handler :: GenerateScriptsParams -> Context () -> IO (Either String GenerateScriptsResponse)
handler GenerateScriptsParams {..} _ = 
  return $ Right $ GenerateScriptsResponse validator mintingPolicy () initialDatum
  where 
    currencySymbol = Gov.qvfSymbol txRef deadline
    regSymbol      = Reg.registrationSymbol currencySymbol
    donSymbol      = Don.donationSymbol regSymbol
    qvfParams      = OC.QVFParams
                      { OC.qvfKeyHolder      = keyHolderPubKeyHash
                      , OC.qvfSymbol         = currencySymbol
                      , OC.qvfProjectSymbol  = regSymbol
                      , OC.qvfDonationSymbol = donSymbol
                      }
    validator = OC.qvfValidator qvfParams
    mintingPolicy = Gov.qvfPolicy txRef deadline
    initialDatum  = RegisteredProjectsCount 0
