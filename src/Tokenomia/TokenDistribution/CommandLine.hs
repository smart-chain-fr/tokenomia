module Tokenomia.TokenDistribution.CommandLine ( execCommand ) where

import Cardano.Api
    ( NetworkId (Mainnet, Testnet)
    , NetworkMagic (..)
    )

import Control.Applicative      ( (<**>), (<|>) )

import Data.Attoparsec.Text     ( parseOnly )
import Data.Text                ( pack )

import Ledger.Value             ( AssetClass )

import Tokenomia.Wallet.Type    ( WalletName )

import Tokenomia.TokenDistribution.Config
import Tokenomia.TokenDistribution.Parser.AssetClass

import Options.Applicative
    ( Parser
    , ParserInfo
    , auto
    , eitherReader
    , flag'
    , fullDesc
    , header
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , progDesc
    , short
    , showDefault
    , strOption
    , switch
    , value
    , execParser
    )

parserConfig :: Parser Config
parserConfig =
        Config
    <$> parserNetworkId
    <*> parserDistributionFile
    <*> parserRecipientPerTx
    <*> parserAssetClass
    <*> parserTokenWallet
    <*> parserAdaWallet
    <*> parserCollateralWallet
    <*> parserMinAda
    <*> parserDryRun
    <*> parserVerbose

opts :: ParserInfo Config
opts =
  info
    (parserConfig <**> helper)
    (fullDesc
        <> header "token-distribution"
        <> progDesc "Distribute tokens to a list of addresses"
    )

parserNetworkId :: Parser NetworkId
parserNetworkId = parserMainnet <|> parserTestnet

parserMainnet :: Parser NetworkId
parserMainnet = flag' Mainnet $
       short 'm'
    <> long "mainnet"
    <> help "Use the mainnet network"

parserTestnet :: Parser NetworkId
parserTestnet = fmap (Testnet . NetworkMagic) $ option auto $
       short 't'
    <> long "testnet"
    <> help "Use this testnet magic id"
    <> showDefault
    <> value 1097911063
    <> metavar "MAGIC"

parserDistributionFile :: Parser FilePath
parserDistributionFile = strOption $
       short 'i'
    <> long "distribution-file"
    <> help "Recipient addresses and amounts to distribute"
    <> metavar "FILENAME"

parserRecipientPerTx :: Parser Int
parserRecipientPerTx = option auto $
       short 'n'
    <> long "recipient-per-tx"
    <> help "Batch together multiple addresses"
    <> showDefault
    <> value 10
    <> metavar "SIZE"

parserAssetClass :: Parser AssetClass
parserAssetClass = option
    (eitherReader (parseOnly assetClassParser . pack)) $
       short 'a'
    <> long "asset-class"
    <> help "Token asset class"
    <> metavar "CURRENCY_SYMBOL.TOKEN_NAME"

parserTokenWallet :: Parser WalletName
parserTokenWallet = strOption $
       short 'T'
    <> long "token-wallet"
    <> help "Wallet name for token source"
    <> metavar "NAME"

parserAdaWallet :: Parser WalletName
parserAdaWallet = strOption $
       short 'A'
    <> long "ada-wallet"
    <> help "Wallet name for Ada source"
    <> metavar "NAME"

parserCollateralWallet :: Parser WalletName
parserCollateralWallet = strOption $
       short 'C'
    <> long "collateral-wallet"
    <> help "Wallet name for collateral"
    <> metavar "NAME"

parserMinAda :: Parser Integer
parserMinAda = option auto $
       short 'e'
    <> long "min-ada-per-utxo"
    <> help "Minimum lovelace amount per UTxO"
    <> showDefault
    <> value 1379280
    <> metavar "AMOUNT"

parserDryRun :: Parser Bool
parserDryRun = switch $
       short 'y'
    <> long "dry-run"
    <> help "Build transactions without submitting them"

parserVerbose :: Parser Bool
parserVerbose = switch $
       short 'v'
    <> long  "verbose"
    <> help  "Show more details about build processes"

execCommand :: IO Config
execCommand = execParser opts
