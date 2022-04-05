module Tokenomia.TokenDistribution.Config (Config (..)) where

import Cardano.Api              ( NetworkId )
import Ledger.Value             ( AssetClass )
import Tokenomia.Wallet.Type    ( WalletName )

data  Config
    = Config
    { configNetworkId :: NetworkId
    , configDistributionFile :: FilePath
    , configRecipientPerTx :: Int
    , configAssetClass :: AssetClass
    , configTokenWallet :: WalletName
    , configAdaWallet :: WalletName
    , configCollateralWallet :: WalletName
    , configMinAdaPerUtxo :: Integer
    , configDryRun :: !Bool
    , configVerbose :: !Bool
    } deriving (Show)
