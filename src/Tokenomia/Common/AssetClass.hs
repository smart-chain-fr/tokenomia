module Tokenomia.Common.AssetClass
  ( adaAssetClass
  ) where

import Ledger.Ada               ( adaSymbol, adaToken )
import Ledger.Value             ( AssetClass, assetClass )


adaAssetClass :: AssetClass
adaAssetClass = assetClass adaSymbol adaToken
