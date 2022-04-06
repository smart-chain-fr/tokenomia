module Tokenomia.TokenDistribution.CLI.Parameters ( Parameters (..) ) where

import Cardano.Api              ( NetworkId )
import Tokenomia.Wallet.Type    ( WalletName )

data  Parameters
    = Parameters
    { networkId :: NetworkId
    , distributionFilePath :: FilePath
    , recipientPerTx :: Int
    , tokenWallet :: WalletName
    , adaWallet :: WalletName
    , collateralWallet :: WalletName
    , minAdaPerUtxo :: Integer
    , dryRun :: Bool
    , verbose :: Bool
    } deriving (Show)
