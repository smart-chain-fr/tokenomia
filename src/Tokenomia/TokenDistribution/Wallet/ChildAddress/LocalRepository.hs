{-# LANGUAGE FlexibleContexts             #-}

module Tokenomia.TokenDistribution.Wallet.ChildAddress.LocalRepository
    ( deriveMissingChildAddresses
    ) where

import Control.Monad.Reader     ( MonadIO, MonadReader )
import Control.Monad.Except     ( MonadError )

import Data.List.NonEmpty       ( toList )
import Data.List                ( (\\) )

import Prelude           hiding ( max )

import Tokenomia.Common.Error       ( TokenomiaError )
import Tokenomia.Common.Environment ( Environment )

import Tokenomia.Wallet.Type        ( WalletName )
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
    ( ChildAddressRef(..)
    , ChildAddressIndex
    )
import Tokenomia.Wallet.ChildAddress.LocalRepository
    ( fetchDerivedChildAddressIndexes
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
