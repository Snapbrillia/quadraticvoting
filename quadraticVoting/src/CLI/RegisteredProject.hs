{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}


module CLI.RegisteredProject where


import qualified Control.Monad.Fail     as M
import qualified Data.Aeson             as A
import           Data.Aeson             ( FromJSON(..)
                                        , (.:) )
import           Data.Foldable          ( find )
import           Data.String            ( fromString )
import qualified Data.Text              as Text
import           GHC.Generics           ( Generic )
import           Plutus.V2.Ledger.Api   as Ledger


data RegisteredProject = RegisteredProject
  { pkh     :: PubKeyHash
  , address :: String
  , tn      :: TokenName
  } deriving (Generic)

instance FromJSON RegisteredProject where
  -- {{{
  parseJSON = A.withObject "RegisteredProject" $ \v -> do
    initPKH <- v .: "pkh"
    initTN  <- v .: "tn"
    case initPKH of
      A.String pkhStr ->
        case initTN of
          A.String tnStr ->
                RegisteredProject
            <$> (return $ fromString $ Text.unpack pkhStr)
            <*> v .: "address"
            <*> (return $ fromString $ Text.unpack tnStr)
          _              ->
            M.fail "Invalid token name."
      _               ->
        M.fail "Invalid public key hash."
  -- }}}


lookupAddressWithPKH :: PubKeyHash -> [RegisteredProject] -> Maybe String
lookupAddressWithPKH pkh' rps = address <$> find ((== pkh') . pkh) rps
