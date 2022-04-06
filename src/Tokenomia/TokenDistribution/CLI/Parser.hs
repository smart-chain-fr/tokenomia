{-# LANGUAGE ImportQualifiedPost          #-}

module Tokenomia.TokenDistribution.CLI.Parser
    ( networkId
    , distributionFilePath
    , recipientPerTx
    , assetClass
    , tokenWallet
    , adaWallet
    , collateralWallet
    , minAda
    , dryRun
    , verbose
    ) where

import Cardano.Api
    ( NetworkId (Mainnet, Testnet)
    , NetworkMagic (..)
    )

import Control.Applicative      ( (<|>) )

import Data.Attoparsec.Text     ( parseOnly )
import Data.Text                ( pack )

import Ledger.Value             ( AssetClass )

import Tokenomia.Wallet.Type    ( WalletName )

import Tokenomia.TokenDistribution.Parser.AssetClass

import Options.Applicative
    ( Parser
    , auto
    , eitherReader
    , flag'
    , help
    , long
    , metavar
    , option
    , short
    , showDefault
    , strOption
    , switch
    , value
    )

networkId :: Parser NetworkId
networkId = mainnet <|> testnet
  where
    mainnet :: Parser NetworkId
    mainnet = flag' Mainnet $
           short 'm'
        <> long "mainnet"
        <> help "Use the mainnet network"
    
    testnet :: Parser NetworkId
    testnet = fmap (Testnet . NetworkMagic) $ option auto $
           short 't'
        <> long "testnet"
        <> help "Use this testnet magic id"
        <> showDefault
        <> value 1097911063
        <> metavar "MAGIC"

distributionFilePath :: Parser FilePath
distributionFilePath = strOption $
       short 'i'
    <> long "distribution-file"
    <> help "Recipient addresses and amounts to distribute"
    <> metavar "FILENAME"

recipientPerTx :: Parser Int
recipientPerTx = option auto $
       short 'n'
    <> long "recipient-per-tx"
    <> help "Batch together multiple addresses"
    <> showDefault
    <> value 10
    <> metavar "SIZE"

assetClass :: Parser AssetClass
assetClass = option (eitherReader (parseOnly assetClassParser . pack)) $
       short 'a'
    <> long "asset-class"
    <> help "Token asset class"
    <> metavar "CURRENCY_SYMBOL.TOKEN_NAME"

tokenWallet :: Parser WalletName
tokenWallet = strOption $
       short 'T'
    <> long "token-wallet"
    <> help "Wallet name for token source"
    <> metavar "NAME"

adaWallet :: Parser WalletName
adaWallet = strOption $
       short 'A'
    <> long "ada-wallet"
    <> help "Wallet name for Ada source"
    <> metavar "NAME"

collateralWallet :: Parser WalletName
collateralWallet = strOption $
       short 'C'
    <> long "collateral-wallet"
    <> help "Wallet name for collateral"
    <> metavar "NAME"

minAda :: Parser Integer
minAda = option auto $
       short 'e'
    <> long "min-ada-per-utxo"
    <> help "Minimum lovelace amount per UTxO"
    <> showDefault
    <> value 1379280
    <> metavar "AMOUNT"

dryRun :: Parser Bool
dryRun = switch $
       short 'y'
    <> long "dry-run"
    <> help "Build transactions without submitting them"

verbose :: Parser Bool
verbose = switch $
       short 'v'
    <> long  "verbose"
    <> help  "Show more details about build processes"
