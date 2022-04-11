{-# LANGUAGE ImportQualifiedPost          #-}

module Tokenomia.TokenDistribution.CLI ( runCommand ) where

import Control.Applicative      ( (<**>) )

import Tokenomia.TokenDistribution.CLI.Parameters
import Tokenomia.TokenDistribution.CLI.Parser qualified as Parser

import Options.Applicative
    ( Parser
    , fullDesc
    , header
    , helper
    , info
    , progDesc
    , execParser
    )

runCommand :: IO Parameters
runCommand = execParser $
    info
        (parser <**> helper)
        (fullDesc
            <> header "token-distribution"
            <> progDesc "Distribute tokens to a list of addresses"
        )
  where
    parser :: Parser Parameters
    parser =
            Parameters
        <$> Parser.networkId
        <*> Parser.distributionFilePath
        <*> Parser.recipientPerTx
        <*> Parser.tokenWallet
        <*> Parser.adaWallet
        <*> Parser.collateralWallet
        <*> Parser.minLovelaces
        <*> Parser.dryRun
        <*> Parser.verbose
