{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE RecordWildCards                           #-}

module Tokenomia.TokenDistribution.Transfer
    ( distributionOutputs
    , transferTokenInParallel
    ) where

import Control.Monad                                   ( void )
import Control.Monad.Except                            ( MonadError )
import Control.Monad.Reader                            ( MonadIO, MonadReader, liftIO )

import Data.Functor.Syntax                             ( (<$$>) )
import Data.List.NonEmpty                              ( NonEmpty((:|)), fromList, toList, (<|) )
import Data.Maybe                                      ( fromJust )

import Prelude hiding                                  ( mapM )

import Ledger.Ada                                      ( lovelaceValueOf )
import Ledger.Value                                    ( Value, assetClassValue )

import Streamly.Prelude                                ( MonadAsync, drain, fromAsync, fromFoldable, mapM )

import Tokenomia.Common.Address                        ( Address(..) )
import Tokenomia.Common.AssetClass                     ( adaAssetClass )
import Tokenomia.Common.Environment                    ( Environment(magicNumber), getNetworkEnvironment )
import Tokenomia.Common.Error                          ( TokenomiaError )

import Tokenomia.Common.Data.Convertible               ( convert )

import Tokenomia.TokenDistribution.CLI.Parameters      ( Parameters(..) )
import Tokenomia.TokenDistribution.Distribution        ( Distribution(..), Recipient(..), countRecipients )


import Tokenomia.Common.Transacting
    ( Fees
    , Metadata(..)
    , TxBalance(..)
    , TxBuild(..)
    , TxInFromWallet(FromWallet)
    , TxOut(ToWallet)
    , build
    , submitWithoutWaitingConfimation
    )

import Tokenomia.Wallet.ChildAddress.ChildAddressRef   ( ChildAddressIndex(..), ChildAddressRef(..) )

import Tokenomia.TokenDistribution.Parser.Address      ( unsafeSerialiseCardanoAddress )
import Tokenomia.TokenDistribution.Wallet.ChildAddress.ChainIndex ( fetchProvisionedUTxO )
import Tokenomia.TokenDistribution.Wallet.ChildAddress.ChildAddressRef ( defaultCollateralAddressRef )
import Tokenomia.TokenDistribution.Wallet.ChildAddress.LocalRepository ( fetchAddressByWalletAtIndex )

import Cardano.Api                                     ( NetworkMagic(NetworkMagic), fromNetworkMagic )

transferTokenInParallel ::
    ( MonadIO m
    , MonadAsync m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => Fees -> Parameters -> NonEmpty Distribution -> m ()
transferTokenInParallel fees parameters distributions =
    drain . fromAsync $
        mapM
            (singleTransfer fees parameters)
            (fromFoldable (zip [1..] (toList distributions)))

singleTransfer ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => Fees -> Parameters -> (ChildAddressIndex, Distribution) -> m ()
singleTransfer fees parameters (index, distribution) = do
        liftIO . print $ "Building " <> show index
        singleTransferInputs parameters index
    >>= maySkip buildAndSubmitWithoutWaitingConfimation
  where
    maySkip :: Applicative m => (a -> m ()) -> Maybe a -> m ()
    maySkip = maybe $ pure ()

    buildAndSubmitWithoutWaitingConfimation ::
        ( MonadIO m
        , MonadReader Environment m
        , MonadError  TokenomiaError m
        )
        => NonEmpty TxInFromWallet -> m ()
    buildAndSubmitWithoutWaitingConfimation inputs = do
            outputs <- distributionOutputs parameters distribution
            build
                (Balanced fees)
                (Just $ defaultCollateralAddressRef $ collateralWallet parameters)
                (singleTransferTxBuild parameters inputs outputs)
        >>= void . submitWithoutWaitingConfimation

singleTransferTxBuild :: Parameters -> NonEmpty TxInFromWallet -> NonEmpty TxOut -> TxBuild
singleTransferTxBuild parameters inputs outputs =
    TxBuild
        { inputsFromScript          = Nothing
        , inputsFromWallet          = inputs
        , outputs                   = outputs
        , validitySlotRangeMaybe    = Nothing
        , metadataMaybe             = Metadata <$> metadataFilePath parameters
        , tokenSupplyChangesMaybe   = Nothing
        }

singleTransferInputs ::
    ( MonadIO m
    , MonadReader Environment m
    )
    => Parameters -> ChildAddressIndex  -> m (Maybe (NonEmpty TxInFromWallet))
singleTransferInputs Parameters{..} index = do
    tokenUTxO <- fetchProvisionedUTxO (ChildAddressRef tokenWallet index)
    adaUTxO   <- fetchProvisionedUTxO (ChildAddressRef adaWallet index)
    return $ FromWallet <$$> sequence (tokenUTxO :| [adaUTxO])

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
distributionOutputs Parameters{..} distribution@Distribution{..} = do
    environment <- getNetworkEnvironment network
    let networkId = fromNetworkMagic $ NetworkMagic $ fromIntegral $ magicNumber environment
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
