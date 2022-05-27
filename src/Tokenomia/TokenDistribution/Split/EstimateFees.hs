{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE ImportQualifiedPost          #-}
{-# LANGUAGE RecordWildCards              #-}

module Tokenomia.TokenDistribution.Split.EstimateFees
    ( estimateFees
    , distributionOutputs
    )
    where

import Control.Monad.Reader     ( MonadIO, MonadReader )
import Control.Monad.Except     ( MonadError )

import Data.Maybe               ( fromJust )
import Data.List.NonEmpty       ( NonEmpty, (<|), fromList, head )

import Ledger.Value             ( Value, assetClassValue )
import Ledger.Ada               ( Ada(..), lovelaceValueOf )

import Tokenomia.Common.Address     ( Address(..) )
import Tokenomia.Common.Error       ( TokenomiaError )
import Tokenomia.Common.Environment ( Environment )
import Tokenomia.Common.AssetClass  ( adaAssetClass )

import Tokenomia.Common.Data.Convertible                ( convert )
import Tokenomia.Common.Data.List.NonEmpty              ( singleton )
import Tokenomia.TokenDistribution.CLI.Parameters       ( Parameters(..) )
import Tokenomia.TokenDistribution.Distribution         ( Distribution(..), Recipient(..), countRecipients )
import Tokenomia.Wallet.ChildAddress.ChildAddressRef    ( ChildAddressRef(..) )

import Tokenomia.Common.Transacting
    ( TxOut(ToWallet)
    , TxInFromWallet(FromWallet)
    , TxBuild(..)
    , TxBalance(..)
    , Metadata(..)
    , mockBuild
    )

import Tokenomia.TokenDistribution.Parser.Address
    ( unsafeSerialiseCardanoAddress )

import Tokenomia.TokenDistribution.Wallet.ChildAddress.ChildAddressRef
    ( defaultFeeAddressRef, defaultCollateralAddressRef )

import Tokenomia.TokenDistribution.Wallet.ChildAddress.ChainIndex
    ( fetchProvisionedUTxO )

import Tokenomia.TokenDistribution.Wallet.ChildAddress.LocalRepository
    ( fetchAddressByWalletAtIndex )


estimateFees ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => Parameters -> NonEmpty Distribution -> m Ada
estimateFees parameters@Parameters{..} distributions = do
    tokenUTxO <- fetchProvisionedUTxO (ChildAddressRef tokenWallet 1)

    let distribution = Data.List.NonEmpty.head distributions
    outputs <- distributionOutputs parameters distribution

    let txbuild = TxBuild
            { inputsFromScript          = Nothing
            , inputsFromWallet          = singleton $ FromWallet . fromJust $ tokenUTxO
            , outputs                   = outputs
            , validitySlotRangeMaybe    = Nothing
            , metadataMaybe             = Metadata <$> metadataFilePath
            , tokenSupplyChangesMaybe   = Nothing
            }

    mockBuild
            (Unbalanced $ defaultFeeAddressRef adaWallet)
            (Just $ defaultCollateralAddressRef collateralWallet)
            txbuild

-- See comments on the splitting of ada source process
-- to understand why a change address is needed there.
--
-- The logic about when to add a change to the outputs
-- must match exactly the conditions defined in `splitAdaSourceOutputs`.
--
-- This holds for the case `n <= 1 || distributeAda`.

distributionOutputs ::
    ( MonadIO m
    , MonadReader Environment m
    )
    => Parameters -> Distribution -> m (NonEmpty TxOut)
distributionOutputs Parameters{..} distribution@Distribution{..} =
    withChangeTxOut . fromList $ zipWith3 ToWallet
        (Address . convert . unsafeSerialiseCardanoAddress networkId . address <$> recipients)
        (addε . assetClassValue assetClass . amount <$> recipients)
        (repeat Nothing)
  where
    ε :: Value
    ε = lovelaceValueOf minLovelacesPerUtxo

    addε :: Value -> Value
    addε
        | distributeAda = id
        | otherwise     = (ε <>)

    distributeAda :: Bool
    distributeAda =
        assetClass == adaAssetClass

    changeTxOut ::
        ( MonadIO m
        , MonadReader Environment m
        )
        => m TxOut
    changeTxOut = do
        changeAddress <- fromJust <$> fetchAddressByWalletAtIndex 0 adaWallet
        pure $ ToWallet changeAddress ε Nothing

    withChangeTxOut ::
        ( MonadIO m
        , MonadReader Environment m
        )
        => NonEmpty TxOut -> m (NonEmpty TxOut)
    withChangeTxOut outputs = do
        let n = countRecipients distribution
        if  n <= 1 || distributeAda
            then (<| outputs) <$> changeTxOut
            else pure outputs
