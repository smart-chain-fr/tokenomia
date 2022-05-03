{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE RecordWildCards              #-}

module Tokenomia.TokenDistribution.Split.EstimateFees
    ( estimateFees
    )
    where

import Control.Monad.Reader     ( MonadIO, MonadReader )
import Control.Monad.Except     ( MonadError )

import Data.List.NonEmpty       ( NonEmpty, fromList, head )

import Ledger.Value             ( Value, assetClassValue )
import Ledger.Ada               ( Ada(..), lovelaceValueOf )

import Tokenomia.Common.Address     ( Address(..) )
import Tokenomia.Common.Error       ( TokenomiaError )
import Tokenomia.Common.Environment ( Environment )
import Tokenomia.Wallet.WalletUTxO  ( WalletUTxO )
import Tokenomia.Wallet.Type        ( WalletName )

import Tokenomia.Common.Data.Convertible                ( convert )
import Tokenomia.Common.Data.List.NonEmpty              ( singleton )
import Tokenomia.TokenDistribution.CLI.Parameters       ( Parameters(..) )
import Tokenomia.TokenDistribution.Distribution         ( Distribution(..), Recipient(..) )
import Tokenomia.Wallet.ChildAddress.ChildAddressRef    ( ChildAddressRef(..) )
import Tokenomia.Wallet.ChildAddress.ChainIndex         ( queryUTxO )

import Tokenomia.Common.Transacting
    ( TxOut(ToWallet)
    , TxInFromWallet(FromWallet)
    , TxBuild(..)
    , TxBalance(..)
    , mockBuild
    )

import Tokenomia.TokenDistribution.Parser.Address
    ( unsafeSerialiseCardanoAddress )
import Tokenomia.TokenDistribution.Wallet.ChildAddress.ChildAddressRef
    ( defaultFeeAddressRef, defaultCollateralAddressRef )

estimateFees ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => Parameters -> NonEmpty Distribution -> m Ada
estimateFees parameters@Parameters{..} distributions = do
    tokenUTxO <- fetchProvisionedTokenUTxO (defaultTokenSource tokenWallet)
    let distribution = Data.List.NonEmpty.head distributions
        txbuild = TxBuild
            { inputsFromScript          = Nothing
            , inputsFromWallet          = singleton $ FromWallet tokenUTxO
            , outputs                   = distributionOutputs parameters distribution
            , validitySlotRangeMaybe    = Nothing
            , metadataMaybe             = Nothing
            , tokenSupplyChangesMaybe   = Nothing
            }

    mockBuild
            (Unbalanced $ defaultFeeAddressRef adaWallet)
            (Just $ defaultCollateralAddressRef collateralWallet)
            txbuild

distributionOutputs :: Parameters -> Distribution -> NonEmpty TxOut
distributionOutputs Parameters{..} Distribution{..} =
    fromList $ zipWith3 ToWallet
        (Address . convert . unsafeSerialiseCardanoAddress networkId . address <$> recipients)
        ((ε <>) . assetClassValue assetClass . amount <$> recipients)
        (repeat Nothing)
  where
    ε :: Value
    ε = lovelaceValueOf minLovelacesPerUtxo

type TokenSource = ChildAddressRef

fetchProvisionedTokenUTxO ::
    ( MonadIO m
    , MonadReader Environment m
    )
    => TokenSource -> m WalletUTxO
fetchProvisionedTokenUTxO tokenSource =
    Prelude.head <$> queryUTxO tokenSource

defaultTokenSource :: WalletName -> TokenSource
defaultTokenSource = flip ChildAddressRef 1
