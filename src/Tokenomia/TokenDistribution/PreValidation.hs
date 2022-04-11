module Tokenomia.TokenDistribution.PreValidation ( preValidation ) where

import Data.List.Unique ( allUnique )

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

preValidation :: Parameters -> Distribution -> Either DistributionError ()
preValidation parameters distribution =
    () <$ sequence (checks <*> pure parameters <*> pure distribution)
  where
    checks :: [Parameters -> Distribution -> Either DistributionError ()]
    checks =
        [ hasRecipient
        , recipientPerTxInRange
        , allAmountsInRange
        , allAddressesUnique
        ]