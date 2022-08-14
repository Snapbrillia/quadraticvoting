-- EXTENSIONS
-- {{{
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- }}}


module Utils where


-- IMPORTS
-- {{{
import           Ledger
import qualified Ledger.Typed.Scripts        as Scripts
import qualified Ledger.Ada                  as Ada
import qualified Plutonomy
import           Plutus.Contract
import           Plutus.V1.Ledger.Credential (Credential (..))
import qualified Plutus.V1.Ledger.Interval   as Interval
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Scripts    (ValidatorHash (..))
import           PlutusTx                    (Data (..))
import qualified PlutusTx
import qualified PlutusTx.AssocMap           as Map
import           PlutusTx.AssocMap           (Map)
import qualified PlutusTx.Builtins           as Builtins
import           PlutusTx.Prelude            hiding (unless)
import           PlutusTx.Prelude            (BuiltinByteString, (<>))
import           PlutusTx.Sqrt               (Sqrt (..), isqrt)
import qualified Prelude                     as P
-- }}}


lovelaceToAda :: Integer -> P.Double
lovelaceToAda lovelace = P.fromIntegral lovelace P./ 1_000_000


{-# INLINABLE pluck #-}
pluck :: (a -> Bool) -> [a] -> Maybe (a, [a])
pluck _ [] = Nothing
pluck p xs =
  -- {{{
  let
    go []       a = a
    go (y : ys) (_, soFar)
      | p y       = (Just y, ys ++ soFar)
      | otherwise = go ys (Nothing, y : soFar)
  in
  case go xs (Nothing, []) of
    (Nothing, _)      -> Nothing
    (Just y, otherYs) -> Just (y, otherYs)
  -- }}}


{-# INLINABLE takeSqrt #-}
-- | Calls `traceError` if a negative input is given.
takeSqrt :: Integer -> Integer
takeSqrt val =
  -- {{{
  case isqrt val of
    Imaginary ->
      traceError "E0"
    Exactly sqrt ->
      sqrt
    Approximately sqrt ->
      sqrt
  -- }}}


{-# INLINABLE lovelaceFromValue #-}
lovelaceFromValue :: Value -> Integer
lovelaceFromValue = Ada.getLovelace . Ada.fromValue


{-# INLINABLE updateIfWith #-}
-- | If an element of the given list satisfies the
--   predicate, that single element is updated,
--   and the new list is returned (applies to the
--   leftmost item only). If no items satify the
--   predicate, @Nothing@ is returned.
updateIfWith :: (a -> Bool) -> (a -> a) -> [a] -> Maybe [a]
updateIfWith predicate fn xs =
  -- {{{
  let
    -- go :: ([a], Maybe a, [a]) -> ([a], Maybe a, [a])
    go acc@([], Nothing, _) = acc
    go acc@(_ , Just _ , _) = acc
    go (p : ps, Nothing, acc)
      | predicate p = (ps, Just $ fn p, acc)
      | otherwise   = go (ps, Nothing, p : acc)
  in
  case go (xs, Nothing, []) of
    (_, Nothing, _) ->
      Nothing
    (leftXs, Just updatedX, rightXs) ->
      Just $ leftXs ++ (updatedX : rightXs)
  -- }}}


-- ERRORS
-- E0  : Square root of a negative number.
-- E1  : Expected an input from own address.
-- E2  : Expected exactly one output to own address.
-- E3  : There should be exactly one authentication token in input.
-- E4  : There should be exactly one authentication token in output.
-- E5  : Deadline passed.
-- E6  : Unauthorized.
-- E7  : No UTxO is sent back to the address.
-- E8  : All outputs should carry datum hashes.
-- E9  : All outputs should carry exactly one authentication token.
-- E10 : All outputs should carry exactly the set minimum Lovelace count.
-- E11 : All outputs should carry mempty datums.
-- E12 : Not all authentication tokens are consumed.
-- E13 : Authentication tokens are not distributed properly.
-- E14 : Funding round not started yet.
-- E15 : Found an input with invalid datum.
-- E16 : All inputs must have exactly one authentication token.
-- E17 : Invalid prize distribution.
-- E18 : Deadline not reached yet.
-- E19 : Improper consumption and production of the authentication tokens.
-- E20 : Key holder not imbursed.
-- E21 : Invalid output datum hash.
-- E22 : Invalid output count.
-- E23 : Project submission fees not paid.
-- E24 : Invalid output datum hash.
-- E25 : Not enough Lovelaces provided.
-- E26 : Invalid output datum hash.
-- E27 : Not enough Lovelaces provided.
-- E28 : Invalid output datum hash.
-- E29 : New deadline is in the past.
-- E30 : This funding round has ended.
-- E31 : Invalid datum/redeemer combination.
-- E32 : No votes cast.
-- E33 : Project already exists.
-- E34 : Donation less than minimum.
-- E35 : Target project not found.
-- E36 : Donation less than minimum.
-- E37 : No datum needed.
-- E38 : Closed state doesn't have minimum Lovelaces.
-- E39 : 
-- E40 : 

