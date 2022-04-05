module Tokenomia.TokenDistribution.Config (Config (..)) where

import Cardano.Api              ( NetworkId )
import Ledger.Value             ( AssetClass )
import Tokenomia.Wallet.Type    ( WalletName )

data  Config
    = Config
      { networkId :: NetworkId
      , distributionFile :: FilePath
      , recipientPerTx :: Int
      , assetClass :: AssetClass
      , tokenWallet :: WalletName
      , adaWallet :: WalletName
      , collateralWallet :: WalletName
      , minAdaPerUtxo :: Integer
      , dryRun :: !Bool
      , verbose :: !Bool
      } deriving (Show)
