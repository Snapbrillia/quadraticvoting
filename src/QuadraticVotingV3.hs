{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- module QuadraticTool () where

import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map as Map
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Constraints (TxConstraints)
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract (ToSchema, ensureKnownCurrencies, printJson, printSchemas, stage)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract
import PlutusTx (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins as Builtins
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Text.Printf (printf)
import Prelude (IO, Semigroup (..), Show, String, show, toInteger, (^))

-- ###### Step 1: Fund Creation Datum (By organization, or whale user) ########################
-- When someone (organization) wants to create a fund, all they basically do is:
-- They send ADA to our contract, which will be locked, with a Datum Attached to that UtxO
-- That datum needs to include the following information
-- ############################################################################################

-- This datum only requires 3 inputs:
-- 1. The fund owner's public address
-- 2. The prize amount they are locking
-- 3. The project labels, which is basically the project category that they prefer to fund
-- 4. The prize distribution ratio which is basically how the fund wants to allocate the prize after the auction is done.
-- (4) i.e maybe we have 3 project winners from 3 different categories. We will not equally distribute them by default
-- (4) maybe the fund wants 40% to go to DeFI project, the rest 40% to AI-Bot, and the final 20% to InsureTech Blockchain

data FundCreationDatum = FundCreationDatum {
         vFundOwner    :: PaymentPubKeyHash,
         vPrizeAmount  :: Integer,
         vProjectLabel :: [String],
         vPrizeDistributionRatio :: [Integer]
            } deriving (Show)

instance Eq FundCreationDatum where
    {-# INLINABLE (==) #-}
    a == b = (vFundOwner              a == vFundOwner   b) &&
             (vPrizeAmount            a == vPrizeAmount b) &&
             (vProjectLabel           a == vProjectLabel   b) &&
             (vPrizeDistributionRatio a == vPrizeDistributionRatio b)

PlutusTx.unstableMakeIsData ''FundCreationDatum
PlutusTx.makeLift ''FundCreationDatum

-- ############################################################################################
-- ############################################################################################

-- ###### Step 2: Project Submission  (by project owners) #####################################
-- This is when a project team comes to the contract and says "Ok, we want to register our project under this fund!"
-- --- "and we want to be eligible for their PrizeAmount"
-- In Cardano terms, this will be one more transaction action where the user will submit their project
-- By submitting their project we basically mean they will lock some funds (i.e we can require 10 ADA for registration)
-- And they will lock the funds with an acompanied DATUM. That datum will include the information we need to specify under which fund it goes.
-- ############################################################################################

data ProjectSubmitDatum = ProjectSubmitDatum {
        vProjectOwner      :: PaymentPubKeyHash,
        vProjectCategory   :: String,
        vFundPayIdentifier :: PaymentPubKeyHash
             } deriving (Show)

instance Eq ProjectSubmitDatum where
    {-# INLINABLE (==) #-}
    b == c = (vProjectOwner           b       == vProjectOwner c) &&
             (vProjectCategory        b       == vProjectCategory c) &&
             (vFundPayIdentifier      b       == vFundPayIdentifier c)

PlutusTx.unstableMakeIsData ''ProjectSubmitDatum
PlutusTx.makeLift ''ProjectSubmitDatum

-- ############################################################################################
-- ############################################################################################

-- ###### Step 3: Voting Action (by voters/donors) ############################################
-- This is the voting action from a user donor. Here it will again be another locking process of funds
-- Users will pay with their ADA, and they will need to include a datum when they do it.
-- THis datum will include: projectToVote, numberOfVotes (which we will calculate if based on their ADA & their project preference it is sufficient to occur)
-- and they will also include their own walletAddress to get refunded when voting process finishes
-- ############################################################################################

data VotingActionDatum = VotingActionDatum {
       vProjectToVote      :: PaymentPubKeyHash,
       vVoterPayAddress    :: PaymentPubKeyHash,
       vNumberOfVotes      :: Integer,
       vAdaLovelaceValue   :: Integer,
       vActionName         :: String
} deriving (Show)

instance Eq VotingActionDatum where
    {-# INLINABLE (==) #-}
    b == c = (vProjectToVote      b       == vProjectToVote c) &&
             (vVoterPayAddress    b       == vVoterPayAddress c) &&
             (vNumberOfVotes      b       == vNumberOfVotes c) &&
             (vAdaLovelaceValue   b       == vAdaLovelaceValue c) &&
             (vActionName         b       == vActionName c)

PlutusTx.unstableMakeIsData ''VotingActionDatum
PlutusTx.makeLift ''VotingActionDatum


-- Explain that properly when push code!!!!
-- SOmeone votes 10 times
-- 5$ 5$ 5 5 5 5 5 5  5 100
-- A. 1 vote - AVG(SUM(VOTES)) 

-- ############################################################################################
-- ############################################################################################

-- Step 4: Putting more money to a fUND #######################################################
-- This is the process when a user/whale/organization adds more funds to an existing fund
-- ############################################################################################

data ConToMatchPool = ConToMatchPool {
        vFundAddress :: PaymentPubKey,
        vPrizeFund   :: Integer
} deriving (Show)

instance Eq ConToMatchPool where
    {-# INLINABLE (==) #-}
    b == c = (vFundAddress      b       == vFundAddress c) &&
             (vPrizeFund        b       == vPrizeFund c) 
-- ############################################################################################
-- ############################################################################################


--- ###IMPORTANT #1    ####
---- ####### How to combine Datum Structures into one main Datum for the validator
-- My idea here is to follow a similar pattern with the Auction contract from PPP
-- BUt it cannot be identical cause we do not use endpoints
-- So my proposal is, to have one createFundDatum with the needed info
-- ANd then create one data RedeemerActions = Vote | ContributeToMatchPool | DistributeFunds -- and each of those constructors will be each datum
-- ANd in our mkVAlidator function to check each of these 3 actions accordingly and based on
-- what it is, include the right conditions
-- ##################################################################################


data QuadraAction = SubProject ProjectSubmitDatum | VoteProject VotingActionDatum | ContribPool ConToMatchPool deriving (Show)

-- | DistribPrize DistributPrizesDatum

PlutusTx.unstableMakeIsData ''QuadraACtion
PlutusTx.makeLift ''QuadraACtion


-- ## IMPORTANT #2########
-- ## Constructing The FInal Overall Datum

data QuadraDatum = QuadraDatum
    { qCreateFund  :: !(Maybe FundCreationDatum),
      qVoting      :: !(Maybe VotingActionDatum),
      qSubProject  :: !(Maybe ProjectSubmitDatum),
      qContrPool   :: !(Maybe ConToMatchPool)
    } deriving P.Show

PlutusTx.unstableMakeIsData ''QuadraDatum
PlutusTx.makeLift ''QuadraDatum

data QuadraVoting
instance Scripts.ValidatorTypes QuadraVoting where
    type instance RedeemerType QuadraVoting = QuadraAction
    type instance DatumType QuadraVoting = QuadraDatum

-- ###################################################################
-- ###################################################################




-- Step 5: Prize allocation ###################################################################
-- This is the most important and trickiest part. We need to develop a function, that we will
-- execute once! At the end of the voting process. So, from all the previous steps mentioned
-- the users who interact with the script are basically creating UtXos under our script address by submitting transactions to it. (be careful the transactions term, im talking in Cardano terms)
-- That function needs to do the followings:
-- 1. Query the Utxos under that specific script address we have
-- 2. We check how many projects we have, under how many project labels
-- 3. Calculate the total amount of Prizes by Category
-- 4. Calculate the voting power of each project (use the quadratic formula)
-- 5. Calculate how much money each project got
-- 6. Allocate the Total MatchPoolPrize to the winning projects based on the ratio and the voting power calculated above
-- 7. Allocate the funds gathered from the Voters to their prefered and voted projects
-- ############################################################################################

-- ############################################################################################
-- ############################################################################################






-- ##################################           *******Previous version code starts here
-- #####################################################################################

-- Flow for this on playground should work as follows
-- 1)One wallet will start the fund with a unique identifier of the fund.
-- 2)Users can enroll into the fund as a project, donate to match pool, and cast votes
-- 3)Finally user can end the fund , and funds from match pooi will be distributed quadratically

-- I used the isuitable function to filter utxos. it worked before the datum was nested like this
-- After changing the datum to below , the function does not work anymore. I think it is problems with acessing
-- nested field values but i can't find any resources on it.The is suitableFunction is under the vote function
data DatumAction = Start StartDatum | MatchPool MatchPoolDatum | Enroll EnrollDatum | Vote VoteDatum deriving (Show)

-- Unique identifier of fund you are staring. Used for things such as enrolling into  a specific fund , ending a specific fund , counting votes etc......
data StartDatum = StartDatum
  { startFund :: Integer
  }
  deriving (Show)

-- Amount to donate to match pool , and the unique identifier of fund you want to donate to
data MatchPoolDatum = MatchDatum
  { matchAmount :: Integer,
    matchFund :: Integer
  }
  deriving (Show)

-- Projects own PaymentKey so the projects can recieve their funds, the fund that the project is enrolling to
data EnrollDatum = EnrollDatum
  { projectPaymentPubKey :: PaymentPubKeyHash,
    enrolledFund :: Integer
  }
  deriving (Show)

-- The paymentkey of project they are voting for , the fund they are voting in , the wallet's own payment to keep track
-- of how many times the wallet has voted for this project in this fund to calculate voting power.
-- Vote datum is Work in progres .......
data VoteDatum = VoteDatum
  { projectPubKey :: PaymentPubKeyHash,
    amount :: Integer,
    fund :: Integer,
    paymentKey :: PaymentPubKeyHash
  }
  deriving (Show)

PlutusTx.unstableMakeIsData ''DatumAction
PlutusTx.unstableMakeIsData ''StartDatum
PlutusTx.unstableMakeIsData ''EnrollDatum
PlutusTx.unstableMakeIsData ''MatchPoolDatum
PlutusTx.unstableMakeIsData ''VoteDatum

-- There needs to be more work done on the validator script. I am not sure if where the logic of some of the code should be located. Should
-- the logic of accepting the vote, determining if they can recieve the fund etc.... be in the validator script , or should it be off-chain in the wallet.
-- Having trouble deciding what part should be on-chain and what part should not , work-in-progress .........
{-# INLINEABLE mkValidator #-}
mkValidator :: DatumAction -> () -> ScriptContext -> Bool
mkValidator dat _ ctx = True

data Voting

instance Scripts.ValidatorTypes Voting where
  type DatumType Voting = DatumAction
  type RedeemerType Voting = ()

typedValidator :: Scripts.TypedValidator Voting
typedValidator =
  Scripts.mkTypedValidator @Voting
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @DatumAction @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

-- Params for each endpoint(action) that the user can do
data StartParams = StartParams
  { spRoundIdentifier :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data VoteParams = VoteParams
  { vpAmount :: !Integer,
    vpFund :: !Integer,
    vpProjectPubKey :: !PaymentPubKeyHash
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MatchParams = MatchParams
  { mpAmount :: !Integer,
    mpFund :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CollectParams = CollectParams
  { cpFund :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data EnrollParams = EnrollParams
  { epFund :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type VoteSchema =
  Endpoint "start" StartParams
    .\/ Endpoint "vote" VoteParams
    .\/ Endpoint "donate to match pool" MatchParams
    .\/ Endpoint "enroll to fund" EnrollParams
    .\/ Endpoint "collect" CollectParams

-- Below is the function to start a fund, it will cost 1 ada to initiate this. Just need to create a unique identifier of the fund they are starting.
start :: forall w s e. AsContractError e => StartParams -> Contract w s e ()
start sp = do
  let dat =
        Start
          StartDatum
            { startFund = spRoundIdentifier sp
            }
      tx = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ 1000000000
  ledgerTx <- submitTxConstraints typedValidator tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ printf "created fund %d" (spRoundIdentifier sp)

-- Needs to add condition in match and enroll function to decline transaction
-- if fund has not been created yet. Work In Progress

-- Funtion for organizations to donate to match pool.
match :: forall w s e. AsContractError e => MatchParams -> Contract w s e ()
match mp = do
  let dat =
        MatchPool
          MatchDatum
            { matchAmount = mpAmount mp,
              matchFund = mpFund mp
            }
      tx = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ mpAmount mp
  ledgerTx <- submitTxConstraints typedValidator tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ printf "matched fund %d" (mpFund mp)

-- Function for project to join the round
enroll :: forall w s e. AsContractError e => EnrollParams -> Contract w s e ()
enroll ep = do
  pkh <- ownPaymentPubKeyHash
  let dat =
        Enroll
          EnrollDatum
            { projectPaymentPubKey = pkh,
              enrolledFund = epFund ep
            }
      tx = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ 1000000000
  ledgerTx <- submitTxConstraints typedValidator tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ printf "enrolled in fund %d" (epFund ep)

-- Function to cast their vote and the project they are voting for.
-- Need to rethink vote function. Do user cast with money , do they vote
-- with tokens that we created as voting power etc ........ Do we caluclate cost of vote first
-- work in progres.....
vote :: forall w s e. AsContractError e => VoteParams -> Contract w s e ()
vote vp = do
  pkh <- ownPaymentPubKeyHash
  previousUtxos <- Map.filter (isSuitable pkh (vpFund vp) (vpProjectPubKey vp)) <$> utxosAt scrAddress
  if (vpAmount vp) /= (((toInteger (Map.size previousUtxos) + 1) ^ 2) * 1000000000)
    then logInfo @String $ printf "not right amount to vote  "
    else do
      let dat =
            Vote
              VoteDatum
                { amount = vpAmount vp,
                  projectPubKey = vpProjectPubKey vp,
                  fund = vpFund vp
                }
          tx = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ vpAmount vp
      ledgerTx <- submitTxConstraints typedValidator tx
      void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
      logInfo @String $ printf "vote success "
  where
    isSuitable :: PaymentPubKeyHash -> Integer -> PaymentPubKeyHash -> ChainIndexTxOut -> Bool
    isSuitable pkh fundRound projectKey o = case _ciTxOutDatum o of
      Left _ -> False
      Right (Datum e) -> case PlutusTx.fromBuiltinData e of
        Nothing -> False
        Just d -> paymentKey d == pkh && fund d == fundRound && projectPubKey d == projectKey

--Function to distribute the winnings , it is under development
collect :: CollectParams -> Bool
collect cp = True

endpoints :: Contract () VoteSchema Text ()
endpoints = awaitPromise (start' `select` vote' `select` match' `select` enroll') >> endpoints
  where
    start' = endpoint @"start" start
    vote' = endpoint @"vote" vote
    match' = endpoint @"donate to match pool" match
    enroll' = endpoint @"enroll to fund" enroll

mkSchemaDefinitions ''VoteSchema
