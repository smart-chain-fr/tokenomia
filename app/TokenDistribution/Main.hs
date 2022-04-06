{-# LANGUAGE ImportQualifiedPost          #-}

module Main ( main ) where

import Tokenomia.TokenDistribution.CLI qualified as CLI
import Tokenomia.TokenDistribution.Distribution

main :: IO ()
main = do
    parameters <- CLI.runCommand
    content <- readDistributionFile parameters

    putStrLn $ show parameters
    putStrLn $ show content
