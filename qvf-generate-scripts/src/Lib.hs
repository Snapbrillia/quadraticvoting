{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib where

import           GHC.Generics
import           Data.Aeson
import           Aws.Lambda
import           Plutus.Contract
import           Ledger                      hiding (mint, singleton, Context)
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value

import qualified OnChain                as OC
import qualified Token

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
  , initialDatum  :: OC.QVFDatum
  } 
  deriving (Generic, FromJSON, ToJSON)

handler :: GenerateScriptsParams -> Context () -> IO (Either String GenerateScriptsResponse)
handler gsp@GenerateScriptsParams {..} context = 
  return $ Right $ GenerateScriptsResponse validator mintingPolicy () initialDatum
  where 
    tokenSymbol  = Token.qvfSymbol txRef authTokenName
    qvfParams    = OC.QVFParams
                    { OC.qvfKeyHolder  = keyHolderPubKeyHash
                    , OC.qvfSymbol     = tokenSymbol
                    , OC.qvfTokenName  = authTokenName
                    }
    validator = OC.qvfValidator qvfParams
    mintingPolicy = Token.qvfPolicy txRef authTokenName
    initialDatum  = OC.initialDatum deadline
