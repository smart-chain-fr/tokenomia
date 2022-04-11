{-# LANGUAGE ImportQualifiedPost          #-}

module Main ( main ) where

import Tokenomia.TokenDistribution.CLI qualified as CLI
import Tokenomia.TokenDistribution.Distribution
import Tokenomia.TokenDistribution.PreValidation

main :: IO ()
main = do
    parameters <- CLI.runCommand
    content <- readDistributionFile parameters

    putStrLn $ show parameters
    putStrLn $ show content
    case content of
        Left _ -> putStrLn "fail"
        Right c -> putStrLn $ show $ preValidation parameters c
