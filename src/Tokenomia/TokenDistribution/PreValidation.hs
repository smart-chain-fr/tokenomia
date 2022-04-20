{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE RecordWildCards              #-}

module Tokenomia.TokenDistribution.PreValidation
    ( preValidation
    , fetchProvisionedTokenUTxO
    ) where

import Control.Monad.Except
import Control.Monad.Reader

import Control.Arrow            ( left )
import Data.List.Unique         ( allUnique )
import Data.List.NonEmpty       ( NonEmpty((:|)) )
import Data.Either.Validation   ( Validation, eitherToValidation )

import Tokenomia.Common.Asset
import Tokenomia.Common.Error
import Tokenomia.Common.Environment
import Tokenomia.Common.Value   ( assetClassValueOfWith, maximumByAssetClassValueOf' )

import Tokenomia.Wallet.CLI
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
import Tokenomia.Wallet.WalletUTxO

import Tokenomia.TokenDistribution.CLI.Parameters  as CLI ( Parameters(..) )
import Tokenomia.TokenDistribution.Distribution    as CLI ( Distribution(..), Recipient(..) )

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
        | CLI.amount recipient <=   0 = Left AmountTooLow
        | otherwise               = Right ()

allAddressesUnique :: Parameters -> Distribution -> Either DistributionError ()
allAddressesUnique _ distribution
    | allUnique addresses = Right ()
    | otherwise           = Left AddressDuplicate
  where
    addresses = CLI.address <$> recipients distribution

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

type TokenSource = ChildAddressRef

-- | Search for an UTxO containing the required amount of an asset class
fetchProvisionedTokenUTxO ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m
    )
    => TokenSource -> Asset -> m WalletUTxO
fetchProvisionedTokenUTxO tokenSource Asset{..} =
        fetchUTxOFilterBy predicate tokenSource
    >>= whenNothingThrow NoUTxOsFound
    >>= \walletUTxOs ->
            return (maximumByAssetClassValueOf' value walletUTxOs assetClass)
  where
    predicate :: WalletUTxO -> Bool
    predicate walletUTxO =
        assetClassValueOfWith (>= amount) (value walletUTxO) assetClass
