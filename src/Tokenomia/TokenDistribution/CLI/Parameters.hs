{-# LANGUAGE DerivingStrategies                        #-}
module Tokenomia.TokenDistribution.CLI.Parameters
    ( Parameters(..)
    ) where

import Cardano.Api                                     ( NetworkId )
import Tokenomia.Wallet.Type                           ( WalletName )

data  Parameters
    = Parameters
    { networkId :: NetworkId
    , distributionFilePath :: FilePath
    , recipientPerTx :: Int
    , tokenWallet :: WalletName
    , adaWallet :: WalletName
    , collateralWallet :: WalletName
    , minLovelacesPerUtxo :: Integer
    , metadataFilePath :: Maybe FilePath
    , dryRun :: Bool
    , verbose :: Bool
    } deriving stock (Show)
