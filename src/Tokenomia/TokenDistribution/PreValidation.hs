{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE ImportQualifiedPost          #-}
{-# LANGUAGE RecordWildCards              #-}

module Tokenomia.TokenDistribution.PreValidation
    ( preValidation
    , tokenSourceProvisionedUTxO
    ) where

import Control.Monad.Except     ( MonadIO )
import Control.Monad.Reader     ( MonadReader )

import Control.Arrow            ( left )
import Data.Composition         ( (.:) )
import Data.List.Unique         ( allUnique )
import Data.List.NonEmpty       ( NonEmpty )
import Data.Either.Combinators  ( maybeToRight )
import Data.Either.Validation   ( Validation, eitherToValidation )

import Tokenomia.Common.AssetClass          ( adaAssetClass )
import Tokenomia.Common.Asset               ( Asset(..) )
import Tokenomia.Common.Environment         ( Environment )
import Tokenomia.Common.Value               ( assetClassValueOfWith, maximumByAssetClassValueOf' )
import Tokenomia.Common.Data.List.NonEmpty  ( singleton )

import Tokenomia.Wallet.CLI                 ( fetchUTxOFilterBy )
import Tokenomia.Wallet.WalletUTxO          ( WalletUTxO, value )

import Tokenomia.Wallet.ChildAddress.ChildAddressRef      ( ChildAddressRef(..) )

import Tokenomia.TokenDistribution.CLI.Parameters  as CLI ( Parameters(..) )
import Tokenomia.TokenDistribution.Distribution    as CLI ( Distribution(..), Recipient(..) )

data DistributionError
    = NoRecipient
    | RecipientPerTxTooLow
    | RecipientPerTxTooHigh
    | AmountTooLow
    | AmountTooHigh
    | AddressDuplicate
    | NoProvisionedAdaSource
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
allAmountsInRange Parameters{..} Distribution{..} =
    () <$ traverse amountInRange recipients
  where
    amountInRange :: Recipient -> Either DistributionError ()
    amountInRange Recipient{..}
        |     distributeAda && amount <= ε  = Left AmountTooLow
        | not distributeAda && amount <= 0  = Left AmountTooLow
        | otherwise                         = Right ()

    distributeAda :: Bool
    distributeAda = assetClass == adaAssetClass

    ε :: Integer
    ε = minLovelacesPerUtxo

allAddressesUnique :: Parameters -> Distribution -> Either DistributionError ()
allAddressesUnique _ distribution
    | allUnique addresses = Right ()
    | otherwise           = Left AddressDuplicate
  where
    addresses = CLI.address <$> recipients distribution

tokenSourceProvisionedUTxO ::
    ( MonadIO m
    , MonadReader Environment m
    )
    => Parameters -> Distribution -> m (Maybe WalletUTxO)
tokenSourceProvisionedUTxO parameters distribution =
    fetchProvisionedUTxO
        (ChildAddressRef (tokenWallet parameters) 0)
        (Asset (CLI.assetClass distribution) totalTokenRequired)
  where
    totalTokenRequired :: Integer
    totalTokenRequired = sum (CLI.amount <$> recipients distribution)

-- TODO: Parse, don't validate (avoid recomputation by not discarding the results)
tokenSourceProvisioned ::
    ( MonadIO m
    , MonadReader Environment m
    )
    => Parameters -> Distribution -> m (Either DistributionError ())
tokenSourceProvisioned parameters distribution =
    maybeToRightUnit <$> tokenSourceProvisionedUTxO parameters distribution
  where
    maybeToRightUnit :: Maybe a -> Either DistributionError ()
    maybeToRightUnit = (() <$) . maybeToRight NoProvisionedTokenSource

adaSourceProvisionedUTxO ::
    ( MonadIO m
    , MonadReader Environment m
    )
    => Parameters -> Distribution -> m (Maybe WalletUTxO)
adaSourceProvisionedUTxO Parameters{..} distribution =
    fetchProvisionedUTxO
        (ChildAddressRef adaWallet 0)
        (Asset adaAssetClass totalAdaRequired)
  where
    totalAdaRequired :: Integer
    totalAdaRequired =
        let n = toInteger . length $ recipients distribution
        in
            n * ε

    ε :: Integer
    ε = minLovelacesPerUtxo

-- TODO: Parse, don't validate (avoid recomputation by not discarding the results)
adaSourceProvisioned ::
    ( MonadIO m
    , MonadReader Environment m
    )
    => Parameters -> Distribution -> m (Either DistributionError ())
adaSourceProvisioned parameters distribution =
    maybeToRightUnit <$> adaSourceProvisionedUTxO parameters distribution
  where
    maybeToRightUnit :: Maybe a -> Either DistributionError ()
    maybeToRightUnit = (() <$) . maybeToRight NoProvisionedAdaSource

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
    , adaSourceProvisioned
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

-- | Search for an UTxO containing the required amount of an asset class
fetchProvisionedUTxO ::
    ( MonadIO m
    , MonadReader Environment m
    )
    => ChildAddressRef -> Asset -> m (Maybe WalletUTxO)
fetchProvisionedUTxO source Asset{..} =
    (fmap . fmap)
        (\walletUTxOs -> maximumByAssetClassValueOf' value walletUTxOs assetClass)
        (fetchUTxOFilterBy predicate source)
  where
    predicate :: WalletUTxO -> Bool
    predicate walletUTxO =
        assetClassValueOfWith (>= amount) (value walletUTxO) assetClass
