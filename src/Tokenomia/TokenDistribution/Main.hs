{-# LANGUAGE FlexibleContexts             #-}

module Tokenomia.TokenDistribution.Main
    ( main
    ) where

import Control.Monad.Reader     ( MonadIO, MonadReader, runReaderT, liftIO )
import Control.Monad.Except     ( MonadError, runExceptT )

import Data.Either.Validation   ( validationToEither )

import Streamly.Prelude         ( MonadAsync )

import Tokenomia.Common.Error
    ( TokenomiaError(InvalidTransaction)
    , whenNothingThrow
    , whenLeftThrow
    )

import Tokenomia.Common.Environment
    ( Environment
    , getNetworkEnvironmment
    )

import Tokenomia.Wallet.CLI                                 ( selectBiggestStrictlyADAsNotCollateral )
import Tokenomia.Wallet.ChildAddress.ChildAddressRef        ( ChildAddressRef(..) )

import Tokenomia.TokenDistribution.CLI                      ( runCommand )
import Tokenomia.TokenDistribution.CLI.Parameters           ( Parameters(..) )
import Tokenomia.TokenDistribution.Distribution             ( readDistributionFile )
import Tokenomia.TokenDistribution.PreValidation            ( preValidation, tokenSourceProvisionedUTxO )

import Tokenomia.TokenDistribution.Split.EstimateFees       ( estimateFees )
import Tokenomia.TokenDistribution.Split.SplitAdaSource     ( splitAdaSource )
import Tokenomia.TokenDistribution.Split.SplitDistribution  ( splitDistribution )
import Tokenomia.TokenDistribution.Split.SplitTokenSource   ( splitTokenSource )

import Tokenomia.TokenDistribution.Transfer                 ( transferTokenInParallel )

import Tokenomia.TokenDistribution.Wallet.ChildAddress.LocalRepository
    ( deriveMissingChildAddresses )

import Tokenomia.TokenDistribution.Wallet.ChildAddress.ChildAddressRef
    ( maxChildAddressIndexRequired )


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
    , MonadAsync m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => Parameters -> m ()
run parameters = do
    distribution <- step "Read distribution file" $
            liftIO (readDistributionFile parameters)
        >>= whenLeftThrow (InvalidTransaction . show)

    step "Run prevalidations" $
            preValidation parameters distribution
        >>= whenLeftThrow (InvalidTransaction . show) . validationToEither

    let distributions = splitDistribution parameters distribution

    step "Derive missing addresses" $
            deriveMissingChildAddresses
                (tokenWallet parameters)
                (maxChildAddressIndexRequired distributions)
        >>  deriveMissingChildAddresses
                (adaWallet parameters)
                (maxChildAddressIndexRequired distributions)

    tokenUTxO <- step "Select provisioned token source" $
            tokenSourceProvisionedUTxO parameters distribution
        >>= whenNothingThrow (InvalidTransaction "No provisioned token source")

    step "Split token source" $
        splitTokenSource tokenUTxO parameters distributions

    adaUTxO <- step "Select provisioned ada source" $
            selectBiggestStrictlyADAsNotCollateral (ChildAddressRef (adaWallet parameters) 0)
        >>= whenNothingThrow (InvalidTransaction "No provisioned ada source")

    fees <- step "Estimate fees" $
        estimateFees parameters distributions

    step "Split ada source" $
        splitAdaSource adaUTxO fees parameters distributions

    step "Transfer token in parallel" $
        transferTokenInParallel fees parameters distributions

    return ()

step ::
    ( MonadIO m
    , MonadAsync m
    )
    => String -> m a -> m a
step title computation = do
    liftIO . putStr $   "[ ] " <> title <> " ... "
    result <- computation
    liftIO . putStr $ "\r[*] " <> title <> " done\n"
    return result
