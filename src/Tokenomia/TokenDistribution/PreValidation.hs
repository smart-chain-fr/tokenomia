{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE RecordWildCards              #-}

module Tokenomia.TokenDistribution.PreValidation
    ( preValidation
    , fetchProvisionedTokenUTxO
    ) where

import Control.Monad.Except     ( MonadIO )
import Control.Monad.Reader     ( MonadReader )

import Control.Arrow            ( left )
import Data.Composition         ( (.:) )
import Data.List.Unique         ( allUnique )
import Data.List.NonEmpty       ( NonEmpty((:|)) )
import Data.Either.Combinators  ( maybeToRight )
import Data.Either.Validation   ( Validation, eitherToValidation )

import Tokenomia.Common.Asset   ( Asset(..) )
import Tokenomia.Common.Environment ( Environment )
import Tokenomia.Common.Value   ( assetClassValueOfWith, maximumByAssetClassValueOf' )

import Tokenomia.Wallet.CLI     ( fetchUTxOFilterBy )
import Tokenomia.Wallet.ChildAddress.ChildAddressRef ( ChildAddressRef(..) )
import Tokenomia.Wallet.WalletUTxO ( WalletUTxO, value )

import Tokenomia.TokenDistribution.CLI.Parameters  as CLI ( Parameters(..) )
import Tokenomia.TokenDistribution.Distribution    as CLI ( Distribution(..), Recipient(..) )

data DistributionError
    = NoRecipient
    | RecipientPerTxTooLow
    | RecipientPerTxTooHigh
    | AmountTooLow
    | AmountTooHigh
    | AddressDuplicate
    | NoProvisionedTokenSource
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

tokenSourceProvisioned ::
    ( MonadIO m
    , MonadReader Environment m
    )
    => Parameters -> Distribution -> m (Either DistributionError ())
tokenSourceProvisioned parameters distribution =
    maybeToRightUnit <$> fetchProvisionedTokenUTxO
        (ChildAddressRef (tokenWallet parameters) 0)
        (Asset (CLI.assetClass distribution) totalTokenRequired)
  where
    totalTokenRequired :: Integer
    totalTokenRequired = sum (CLI.amount <$> recipients distribution)

    maybeToRightUnit :: Maybe a -> Either DistributionError ()
    maybeToRightUnit = (() <$) . maybeToRight NoProvisionedTokenSource

checks ::
    ( MonadIO m
    , MonadReader Environment m
    )
    => [Parameters -> Distribution -> m (Either DistributionError ())]
checks =
    [ return .: hasRecipient
    , return .: recipientPerTxInRange
    , return .: allAmountsInRange
    , return .: allAddressesUnique
    , tokenSourceProvisioned
    ]

preValidation ::
    ( MonadIO m
    , MonadReader Environment m
    )
    => Parameters -> Distribution -> m (Validation (NonEmpty DistributionError) ())
preValidation parameters distribution =
    toValidation <$> sequencedChecks
  where
    sequencedChecks ::
        ( MonadIO m
        , MonadReader Environment m
        )
        => m [Either DistributionError ()]
    sequencedChecks = sequence $ checks <*> pure parameters <*> pure distribution

    toValidation ::
        [Either DistributionError ()] -> Validation (NonEmpty DistributionError) ()
    toValidation xs = () <$ traverse (eitherToValidation . left singleton) xs

    singleton :: a -> NonEmpty a
    singleton = (:| [])

type TokenSource = ChildAddressRef

-- | Search for an UTxO containing the required amount of an asset class
fetchProvisionedTokenUTxO ::
    ( MonadIO m
    , MonadReader Environment m
    )
    => TokenSource -> Asset -> m (Maybe WalletUTxO)
fetchProvisionedTokenUTxO tokenSource Asset{..} =
    (fmap . fmap)
        (\walletUTxOs -> maximumByAssetClassValueOf' value walletUTxOs assetClass)
        (fetchUTxOFilterBy predicate tokenSource)
  where
    predicate :: WalletUTxO -> Bool
    predicate walletUTxO =
        assetClassValueOfWith (>= amount) (value walletUTxO) assetClass
