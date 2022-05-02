{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE RecordWildCards              #-}

module Tokenomia.TokenDistribution.Split.SplitTokenSource
    ( splitTokenSource
    ) where

import Prelude           hiding ( repeat, tail, zipWith3 )

import Control.Monad.Reader     ( MonadIO, MonadReader )
import Control.Monad.Except     ( MonadError )

import Data.List.NonEmpty       ( NonEmpty, fromList, repeat, tail )

import Ledger.Ada               ( lovelaceValueOf )
import Ledger.Value             ( Value, assetClassValue )

import Tokenomia.Common.Address     ( Address )
import Tokenomia.Common.Error       ( TokenomiaError )
import Tokenomia.Common.Environment ( Environment )
import Tokenomia.Wallet.WalletUTxO  ( WalletUTxO )
import Tokenomia.Wallet.Type        ( WalletName )

import Tokenomia.Common.Transacting
    ( TxInFromWallet(..)
    , TxOut(..)
    , TxBuild(..)
    , TxBalance(..)
    , buildAndSubmit
    )

import Tokenomia.Common.Data.List.NonEmpty          ( singleton, zipWith3 )

import Tokenomia.TokenDistribution.CLI.Parameters   ( Parameters(..) )
import Tokenomia.TokenDistribution.Distribution     ( Distribution(..), Recipient(..) )

import Tokenomia.TokenDistribution.Wallet.ChildAddress.LocalRepository
    ( fetchAddressesByWallet )

import Tokenomia.TokenDistribution.Wallet.ChildAddress.ChildAddressRef
    ( defaultFeeAddressRef, defaultCollateralAddressRef )


splitTokenSource ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => WalletUTxO -> Parameters -> NonEmpty Distribution -> m ()
splitTokenSource source parameters distributions = do
    splitTokenSourceTxBuild source parameters distributions >>=
        buildAndSubmit
            (Unbalanced $ defaultFeeAddressRef $ adaWallet parameters)
            (Just $ defaultCollateralAddressRef $ collateralWallet parameters)

splitTokenSourceTxBuild ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => WalletUTxO -> Parameters -> NonEmpty Distribution -> m TxBuild
splitTokenSourceTxBuild source parameters distributions = do
    outputs <- splitTokenSourceOutputs parameters distributions
    return TxBuild
        { inputsFromScript          = Nothing
        , inputsFromWallet          = singleton $ FromWallet source
        , outputs                   = outputs
        , validitySlotRangeMaybe    = Nothing
        , metadataMaybe             = Nothing
        , tokenSupplyChangesMaybe   = Nothing
        }

splitTokenSourceOutputs ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => Parameters -> NonEmpty Distribution -> m (NonEmpty TxOut)
splitTokenSourceOutputs Parameters{..} distributions = do
    addresses <- fetchOutputAddressesByWallet tokenWallet
    let values = tokenSum <$> distributions
    return $ zipWith3 ToWallet addresses values (repeat Nothing)
  where
    tokenSum :: Distribution -> Value
    tokenSum Distribution{..} =
            assetClassValue assetClass (sum (amount <$> recipients))
        <>  ε

    ε :: Value
    ε = lovelaceValueOf minLovelacesPerUtxo

fetchOutputAddressesByWallet ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => WalletName -> m (NonEmpty Address)
fetchOutputAddressesByWallet walletName =
    fromList . tail <$> fetchAddressesByWallet walletName
