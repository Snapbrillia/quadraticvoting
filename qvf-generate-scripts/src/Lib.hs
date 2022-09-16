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
import qualified OnChain              as OC
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
handler GenerateScriptsParams {..} _ = 
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
