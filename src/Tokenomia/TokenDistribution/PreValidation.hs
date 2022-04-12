module Tokenomia.TokenDistribution.PreValidation ( preValidation ) where

import Control.Arrow            ( left )
import Data.List.Unique         ( allUnique )
import Data.List.NonEmpty       ( NonEmpty((:|)) )
import Data.Either.Validation   ( Validation, eitherToValidation )

import Tokenomia.TokenDistribution.CLI.Parameters   ( Parameters(..) )
import Tokenomia.TokenDistribution.Distribution     ( Distribution(..), Recipient(..) )

data DistributionError
    = NoRecipient
    | RecipientPerTxTooLow
    | RecipientPerTxTooHigh
    | AmountTooLow
    | AmountTooHigh
    | AddressDuplicate
    deriving (Show)

hasRecipient :: Parameters -> Distribution -> Either DistributionError ()
hasRecipient _ distribution
    | null . recipients $ distribution = Left NoRecipient
    | otherwise                        = Right ()

recipientPerTxInRange :: Parameters -> Distribution -> Either DistributionError ()
recipientPerTxInRange parameters _
    | recipientPerTx parameters <= 0 = Left RecipientPerTxTooLow
    | recipientPerTx parameters > 70 = Left RecipientPerTxTooHigh
    | otherwise                      = Right ()

allAmountsInRange :: Parameters -> Distribution -> Either DistributionError ()
allAmountsInRange _ distribution =
    () <$ traverse amountInRange (recipients distribution)
  where
    amountInRange :: Recipient -> Either DistributionError ()
    amountInRange recipient
        | amount recipient <=   0 = Left AmountTooLow
        | otherwise               = Right ()

allAddressesUnique :: Parameters -> Distribution -> Either DistributionError ()
allAddressesUnique _ distribution
    | allUnique addresses = Right ()
    | otherwise           = Left AddressDuplicate
  where
    addresses = address <$> recipients distribution

preValidation :: Parameters -> Distribution -> Validation (NonEmpty DistributionError) ()
preValidation parameters distribution =
    () <$ traverse
      (eitherToValidation . left singleton)
      (checks <*> pure parameters <*> pure distribution)
  where
    singleton :: a -> NonEmpty a
    singleton = (:| [])

    checks :: [Parameters -> Distribution -> Either DistributionError ()]
    checks =
        [ hasRecipient
        , recipientPerTxInRange
        , allAmountsInRange
        , allAddressesUnique
        ]