{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger.TimeSlot
import Plutus.Trace
import Wallet.Emulator.Wallet


test :: IO ()
test = runEmulatorTraceIO $ do
    let w1 = knownWallet 1
        w2 = knownWallet 2
        w3 = knownWallet 3
        w4 = knownWallet 4
        w5 = knownWallet 5
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    h3 <- activateContractWallet w3 endpoints
    h4 <- activateContractWallet w4 endpoints
    h5 <- activateContractWallet w5 endpoints
    callEndpoint @"launch"     h1 $ LaunchParams  
        { lpKeyHolder = mockWalletPaymentPubKeyHash $ w1
        , lpOpeningTime = 1654041600	
        , lpClosingTime = 1656633599	
        , lpMinDonation = 2000000
        , lpCurrency = "snapbrillia"
        , lpToken = "SnapbrilliaToken"
        }
    void $ Emulator.waitNSlots 10
    callEndpoint @"open"       h1 $ OpenParams       undefined
    void $ Emulator.waitNSlots 10
    callEndpoint @"addProject" h2 $ AddProjectParams 
        { apProject = "Open-Source"
        , apCurrency = "opensource"
        , apToken = "opensourceToken"
        }
    void $ Emulator.waitNSlots 10
    callEndpoint @"donate"     h3 $ AddProjectParams 
        { apProject = "Artifical-Intelligence"
        , apCurrency ="artificalintelligence"
        , apToken = "AIToken"
        }
    void $ Emulator.waitNSlots 10
    callEndpoint @"donate"     h4 $ DonationParams   
        { dpDonation = 9000000
        , dpCurrency = "opensource"
        , dpToken = "opensourceToken"
        }
    void $ Emulator.waitNSlots 10
    callEndpoint @"donate"     h4 $ DonationParams   
        { dpDonation = 4000000
        , dpCurrency = "artificalintelligence"
        , dpToken = "AiToken"
        }
    void $ Emulator.waitNSlots 10
    callEndpoint @"donate"     h5 $ DonationParams   
        { dpDonation = 4000000
        , dpCurrency = "opensource"
        , dpToken = "opensourceToken"
        }
    void $ Emulator.waitNSlots 10
    callEndpoint @"donate"     h1 $ CloseParams   undefined
    void $ Emulator.waitNSlots 10