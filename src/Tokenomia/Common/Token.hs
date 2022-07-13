{-# LANGUAGE RecordWildCards #-}

module Tokenomia.Common.Token (
  Token (..),
  getMinimumUTxOAdaRequired,
) where

import Plutus.V1.Ledger.Ada
import Plutus.V1.Ledger.Value hiding (assetClass)
import Tokenomia.ICO.Balanceable

data Token = Token
  { assetClass :: AssetClass
  , amount :: Integer
  , minimumAdaRequired :: Ada
  }
  deriving stock (Eq, Ord, Show)

instance AdaBalanceable Token where
  adaBalance Token {..} = minimumAdaRequired

instance TokenBalanceable Token where
  tokenBalance Token {..} = amount

getMinimumUTxOAdaRequired :: Ada
getMinimumUTxOAdaRequired = 1379280
