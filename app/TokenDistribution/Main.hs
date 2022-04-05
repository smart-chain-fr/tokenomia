{-# LANGUAGE ImportQualifiedPost          #-}

module Main ( main ) where

import Tokenomia.TokenDistribution.CommandLine qualified

main :: IO ()
main = do
    config <- Tokenomia.TokenDistribution.CommandLine.execCommand
    putStrLn $ show config
