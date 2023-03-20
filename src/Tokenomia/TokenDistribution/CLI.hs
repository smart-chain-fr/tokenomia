{-# LANGUAGE ImportQualifiedPost                       #-}

module Tokenomia.TokenDistribution.CLI
    ( runCommand
    ) where

import Control.Applicative                             ( (<**>) )

import Tokenomia.TokenDistribution.CLI.Parameters      ( Parameters(..) )
import Tokenomia.TokenDistribution.CLI.Parser qualified as Parser

import Options.Applicative                             ( Parser, execParser, fullDesc, header, helper, info, progDesc )

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
        <$> Parser.network
        <*> Parser.distributionFilePath
        <*> Parser.recipientPerTx
        <*> Parser.tokenWallet
        <*> Parser.adaWallet
        <*> Parser.collateralWallet
        <*> Parser.minLovelaces
        <*> Parser.metadataFilePath
        <*> Parser.dryRun
        <*> Parser.verbose
