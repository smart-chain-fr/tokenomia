module Tokenomia.TokenDistribution.Main
    ( main
    ) where

import Control.Monad.Reader     ( ReaderT(runReaderT) )
import Control.Monad.Except     ( runExceptT )

import Tokenomia.Common.Environment

import Tokenomia.TokenDistribution.CLI
import Tokenomia.TokenDistribution.CLI.Parameters
import Tokenomia.TokenDistribution.Distribution
import Tokenomia.TokenDistribution.PreValidation
import Tokenomia.TokenDistribution.Split.SplitDistribution
import Tokenomia.TokenDistribution.Wallet.ChildAddress.LocalRepository
import Tokenomia.TokenDistribution.Wallet.ChildAddress.ChildAddressRef


main :: IO ()
main = do
    parameters <- runCommand
    distribution <- either error id <$> readDistributionFile parameters

    environment <- getNetworkEnvironmment (networkId parameters)
    result <- runReaderT
        ( preValidation parameters distribution
        ) environment
    print result

    print $ length . recipients <$> splitDistribution parameters distribution

    result <- runExceptT $ runReaderT
        ( deriveMissingChildAddresses "TestWallet"
            $ maxChildAddressIndexRequired
                $ splitDistribution parameters distribution
        ) environment
    print result
