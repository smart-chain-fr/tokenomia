{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE RecordWildCards              #-}

module Tokenomia.TokenDistribution.Wallet.ChildAddress.LocalRepository
    ( deriveMissingChildAddresses
    , fetchAddressesByWallet
    , fetchAddressesByWalletWithIndexFilter
    , fetchAddressesByWalletWithNonZeroIndex
    ) where

import Control.Monad.Reader     ( MonadIO, MonadReader )
import Control.Monad.Except     ( MonadError )

import Data.List.NonEmpty       ( NonEmpty, filter, toList )
import Data.List                ( (\\) )

import Prelude           hiding ( filter, max )

import Tokenomia.Common.Address     ( Address )
import Tokenomia.Common.Error       ( TokenomiaError )
import Tokenomia.Common.Environment ( Environment )

import Tokenomia.Wallet.Type        ( WalletName )
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
    ( ChildAddressRef(..)
    , ChildAddressIndex
    )
import Tokenomia.Wallet.ChildAddress.LocalRepository
    ( ChildAddress(..)
    , fetchDerivedChildAddressIndexes
    , fetchById
    , deriveChildAddress
    )


missingChildAddressIndex ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m
    )
    => WalletName -> ChildAddressIndex -> m [ChildAddressIndex]
missingChildAddressIndex walletName max = do
    derivedChildAddressIndexes <- fetchDerivedChildAddressIndexes walletName
    return $ missingUntil max $ toList derivedChildAddressIndexes
  where
    missingUntil :: (Enum a, Num a, Ord a) => a -> [a] -> [a]
    missingUntil n xs = [0..n] \\ xs

missingChildAddressRef ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m
    )
    => WalletName -> ChildAddressIndex -> m [ChildAddressRef]
missingChildAddressRef walletName max =
    (fmap . fmap)
        (ChildAddressRef walletName)
        (missingChildAddressIndex walletName max)

deriveMissingChildAddresses ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m
    )
    => WalletName -> ChildAddressIndex -> m ()
deriveMissingChildAddresses walletName max =
    missingChildAddressRef walletName max >>= mapM_ deriveChildAddress

fetchAddressByChildAddressRef ::
    ( MonadIO m
    , MonadReader Environment m
    )
    => ChildAddressRef -> m Address
fetchAddressByChildAddressRef childAddressRef =
    address <$> fetchById childAddressRef

fetchAddressByWalletAtIndexes ::
    ( MonadIO m
    , MonadReader Environment m
    , Traversable t
    )
    => t ChildAddressIndex -> WalletName -> m (t Address)
fetchAddressByWalletAtIndexes indexes walletName =
    mapM fetchAddressByChildAddressRef (ChildAddressRef walletName <$> indexes)

fetchAddressesByWallet ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => WalletName -> m (NonEmpty Address)
fetchAddressesByWallet walletName = do
    indexes <- fetchDerivedChildAddressIndexes walletName
    fetchAddressByWalletAtIndexes indexes walletName

fetchAddressesByWalletWithIndexFilter ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => (ChildAddressIndex -> Bool) -> WalletName -> m [Address]
fetchAddressesByWalletWithIndexFilter predicate walletName = do
    indexes <- filter predicate <$> fetchDerivedChildAddressIndexes walletName
    fetchAddressByWalletAtIndexes indexes walletName

fetchAddressesByWalletWithNonZeroIndex ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => WalletName -> m [Address]
fetchAddressesByWalletWithNonZeroIndex =
    fetchAddressesByWalletWithIndexFilter (/= 0)
