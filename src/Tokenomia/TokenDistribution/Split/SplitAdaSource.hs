{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE RecordWildCards              #-}

module Tokenomia.TokenDistribution.Split.SplitAdaSource
    ( splitAdaSource
    ) where

import Prelude           hiding ( repeat, head, zipWith3 )

import Control.Monad.Reader     ( MonadIO, MonadReader )
import Control.Monad.Except     ( MonadError )

import Data.List.NonEmpty       ( NonEmpty, fromList, repeat, head )

import Ledger.Ada               ( Ada, lovelaceValueOf, toValue )
import Ledger.Value             ( Value )

import Tokenomia.Common.Error       ( TokenomiaError )
import Tokenomia.Common.Environment ( Environment )
import Tokenomia.Wallet.WalletUTxO  ( WalletUTxO )

import Tokenomia.Common.Transacting
    ( TxInFromWallet(..)
    , TxOut(..)
    , TxBuild(..)
    , TxBalance(..)
    , Metadata(..)
    , buildAndSubmit
    )

import Tokenomia.Common.Data.List.NonEmpty          ( singleton, zipWith3 )

import Tokenomia.TokenDistribution.CLI.Parameters   ( Parameters(..) )
import Tokenomia.TokenDistribution.Distribution     ( Distribution(..) )

import Tokenomia.TokenDistribution.Wallet.ChildAddress.LocalRepository
    ( fetchAddressesByWalletWithIndexInRange )

import Tokenomia.TokenDistribution.Wallet.ChildAddress.ChildAddressRef
    ( defaultFeeAddressRef, defaultCollateralAddressRef )


splitAdaSource ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => WalletUTxO -> Ada -> Parameters -> NonEmpty Distribution -> m ()
splitAdaSource source fees parameters distributions = do
    splitAdaSourceTxBuild source fees parameters distributions >>=
        buildAndSubmit
            (Unbalanced $ defaultFeeAddressRef $ adaWallet parameters)
            (Just $ defaultCollateralAddressRef $ collateralWallet parameters)

splitAdaSourceTxBuild ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => WalletUTxO -> Ada -> Parameters -> NonEmpty Distribution -> m TxBuild
splitAdaSourceTxBuild source fees parameters distributions = do
    outputs <- splitAdaSourceOutputs fees parameters distributions
    return TxBuild
        { inputsFromScript          = Nothing
        , inputsFromWallet          = singleton $ FromWallet source
        , outputs                   = outputs
        , validitySlotRangeMaybe    = Nothing
        , metadataMaybe             = Metadata <$> metadataFilePath parameters
        , tokenSupplyChangesMaybe   = Nothing
        }

splitAdaSourceOutputs ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => Ada -> Parameters -> NonEmpty Distribution -> m (NonEmpty TxOut)
splitAdaSourceOutputs fees Parameters{..} distributions = do
    let range = [1..(length distributions)]
    addresses <- fromList <$> fetchAddressesByWalletWithIndexInRange range adaWallet

    return $ zipWith3 ToWallet addresses values (repeat Nothing)
  where
    nε :: Integer -> Value
    nε n = lovelaceValueOf (n * minLovelacesPerUtxo)

    value :: Integer -> Value
    value n
        | n > 1     = toValue fees <> nε (n - 1)
        | otherwise = toValue fees

    values :: NonEmpty Value
    values =
        let ns = toInteger . length . recipients <$> distributions
        in  value <$> ns
