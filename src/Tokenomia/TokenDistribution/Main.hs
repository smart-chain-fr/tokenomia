{-# LANGUAGE FlexibleContexts             #-}

module Tokenomia.TokenDistribution.Main
    ( main
    ) where

import Control.Monad.Reader     ( MonadIO, MonadReader, ReaderT(runReaderT) )
import Control.Monad.Except     ( MonadError, runExceptT )
import Control.Monad.IO.Class

import Data.Maybe               (fromJust )

import Tokenomia.Common.Error       ( TokenomiaError )
import Tokenomia.Common.Environment ( Environment )
import Tokenomia.Common.Environment

import Tokenomia.TokenDistribution.CLI
import Tokenomia.TokenDistribution.CLI.Parameters
import Tokenomia.TokenDistribution.Distribution
import Tokenomia.TokenDistribution.PreValidation
import Tokenomia.TokenDistribution.Split.SplitDistribution
import Tokenomia.TokenDistribution.Wallet.ChildAddress.LocalRepository
import Tokenomia.TokenDistribution.Wallet.ChildAddress.ChildAddressRef
import Tokenomia.TokenDistribution.Split.SplitTokenSource


main :: IO ()
main = do
    parameters  <- runCommand
    environment <- getNetworkEnvironmment (networkId parameters)

    result      <- runExceptT $ runReaderT
        (run parameters)
        environment
    print result

run ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => Parameters -> m ()
run parameters = do
    distribution <- either error id <$> liftIO (readDistributionFile parameters)

    preValidation parameters distribution >>= liftIO . print

    let distributions = splitDistribution parameters distribution
    liftIO . print $ length . recipients <$> distributions

    walletUTxO <- fromJust <$> tokenSourceProvisionedUTxO parameters distribution
    liftIO . print $ walletUTxO

    splitTokenSource walletUTxO parameters distributions
    return ()
