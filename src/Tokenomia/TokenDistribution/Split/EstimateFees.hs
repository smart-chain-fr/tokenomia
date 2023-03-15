{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE RecordWildCards                           #-}

module Tokenomia.TokenDistribution.Split.EstimateFees
    ( estimateFees
    ) where

import Control.Monad.Except                            ( MonadError )
import Control.Monad.Reader                            ( MonadIO, MonadReader )

import Data.List.NonEmpty                              ( NonEmpty, head )
import Data.Maybe                                      ( fromJust )

import Ledger.Ada                                      ( Ada(..) )

import Tokenomia.Common.Environment                    ( Environment )
import Tokenomia.Common.Error                          ( TokenomiaError )

import Tokenomia.Common.Data.List.NonEmpty             ( singleton )
import Tokenomia.TokenDistribution.CLI.Parameters      ( Parameters(..) )
import Tokenomia.TokenDistribution.Distribution        ( Distribution(..) )
import Tokenomia.Wallet.ChildAddress.ChildAddressRef   ( ChildAddressRef(..) )

import Tokenomia.Common.Transacting
    ( Metadata(..)
    , TxBalance(..)
    , TxBuild(..)
    , TxInFromWallet(FromWallet)
    , mockBuild
    )

import Tokenomia.TokenDistribution.Wallet.ChildAddress.ChildAddressRef
    ( defaultCollateralAddressRef
    , defaultFeeAddressRef
    )

import Tokenomia.TokenDistribution.Wallet.ChildAddress.ChainIndex ( fetchProvisionedUTxO )

import Tokenomia.TokenDistribution.Transfer            ( distributionOutputs )

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
