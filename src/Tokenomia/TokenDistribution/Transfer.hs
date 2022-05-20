{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE ImportQualifiedPost          #-}
{-# LANGUAGE RecordWildCards              #-}

module Tokenomia.TokenDistribution.Transfer
    ( transferTokenInParallel
    ) where

import Control.Monad (void)
import Control.Monad.Reader     ( MonadIO, MonadReader, liftIO )
import Control.Monad.Except     ( MonadError )

import Data.List.NonEmpty       ( NonEmpty((:|)), toList )
import Data.Functor.Syntax      ( (<$$>) )

import Prelude hiding           ( repeat, mapM )

import Streamly.Prelude
    ( MonadAsync
    , drain
    , fromAsync
    , fromFoldable
    , mapM
    )

import Tokenomia.Common.Error       ( TokenomiaError )
import Tokenomia.Common.Environment ( Environment )

import Tokenomia.TokenDistribution.CLI.Parameters   ( Parameters(..) )
import Tokenomia.TokenDistribution.Distribution     ( Distribution(..) )

import Tokenomia.Common.Transacting
    ( TxInFromWallet(..)
    , TxBuild(..)
    , TxBalance(..)
    , Fees
    , build
    , submitWithoutWaitingConfimation
    )

import Tokenomia.Wallet.ChildAddress.ChildAddressRef
    ( ChildAddressIndex(..)
    , ChildAddressRef(..)
    )

import Tokenomia.TokenDistribution.Wallet.ChildAddress.ChildAddressRef
    ( defaultCollateralAddressRef )

import Tokenomia.TokenDistribution.Wallet.ChildAddress.ChainIndex
    ( fetchProvisionedUTxO )

import Tokenomia.TokenDistribution.Split.EstimateFees
    ( distributionOutputs )


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
    singleTransferTxBuild parameters (index, distribution)
    >>= maybe
            (pure ())
            buildAndSubmitWithoutWaitingConfimation
  where
    buildAndSubmitWithoutWaitingConfimation ::
        ( MonadIO m
        , MonadReader Environment m
        , MonadError  TokenomiaError m
        )
        => TxBuild -> m ()
    buildAndSubmitWithoutWaitingConfimation txBuild =
            build
                (Balanced fees)
                (Just $ defaultCollateralAddressRef $ collateralWallet parameters)
                txBuild
        >>= void . submitWithoutWaitingConfimation

singleTransferTxBuild ::
    ( MonadIO m
    , MonadReader Environment m
    )
    => Parameters -> (ChildAddressIndex, Distribution) -> m (Maybe TxBuild)
singleTransferTxBuild parameters (index, distribution) =
         singleTransferTxBuild'
    <$$> singleTransferInputs parameters index
    where
        singleTransferTxBuild' :: NonEmpty TxInFromWallet -> TxBuild
        singleTransferTxBuild' inputs =
            TxBuild
                { inputsFromScript          = Nothing
                , inputsFromWallet          = inputs
                , outputs                   = distributionOutputs parameters distribution
                , validitySlotRangeMaybe    = Nothing
                , metadataMaybe             = Nothing
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
