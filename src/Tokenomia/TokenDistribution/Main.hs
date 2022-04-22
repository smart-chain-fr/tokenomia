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


main :: IO ()
main = do
    parameters <- runCommand
    distribution <- either error id <$> readDistributionFile parameters

    environment <- getNetworkEnvironmment (networkId parameters)
    result <- runReaderT
        ( preValidation parameters distribution
        ) environment
    print result
