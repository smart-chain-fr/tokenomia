{-# LANGUAGE LambdaCase                   #-}

module Tokenomia.TokenDistribution.Main
    ( main
    ) where

import Control.Monad.Reader     ( ReaderT(runReaderT) )
import Control.Monad.Except     ( runExceptT )

import Tokenomia.Common.Asset
import Tokenomia.Common.Environment
import Tokenomia.Wallet.ChildAddress.ChildAddressRef

import Tokenomia.TokenDistribution.CLI
import Tokenomia.TokenDistribution.CLI.Parameters
import Tokenomia.TokenDistribution.Distribution
import Tokenomia.TokenDistribution.PreValidation

main :: IO ()
main = do
    parameters <- runCommand
    content <- either (error "") id <$> readDistributionFile parameters
    print $ Tokenomia.TokenDistribution.Distribution.assetClass content
    environment <- getNetworkEnvironmment (networkId parameters)
    result <- runExceptT $ runReaderT
        ( validateTokenProvisioning
            (ChildAddressRef "CustomToken" 0)
            (Asset (Tokenomia.TokenDistribution.Distribution.assetClass content) 10)
        ) environment
    print result