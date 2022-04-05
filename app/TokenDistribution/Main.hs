{-# LANGUAGE ImportQualifiedPost          #-}

module Main ( main ) where

import Tokenomia.TokenDistribution.CommandLine qualified
import Tokenomia.TokenDistribution.Distribution qualified

main :: IO ()
main = do
    config <- Tokenomia.TokenDistribution.CommandLine.execCommand
    content <- Tokenomia.TokenDistribution.Distribution.readDistributionFile config

    putStrLn $ show config
    putStrLn $ show content
