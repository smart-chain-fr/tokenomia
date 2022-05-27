{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE ImportQualifiedPost          #-}
{-# LANGUAGE RecordWildCards              #-}

module Tokenomia.TokenDistribution.Transfer
    ( transferTokenInParallel
    ) where

import Control.Monad            ( void )
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
    , TxOut(..)
    , TxBuild(..)
    , TxBalance(..)
    , Metadata(..)
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
