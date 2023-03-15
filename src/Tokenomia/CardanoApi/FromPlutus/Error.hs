{-# LANGUAGE DerivingStrategies                        #-}

module Tokenomia.CardanoApi.FromPlutus.Error
    ( FromPlutusError(..)
    ) where


data FromPlutusError
    = PlutusCurrencySymbolNotPolicyId
    | PlutusTokenNameNotAssetName
    | PlutusAssetClassNotAssetId
    deriving stock (Show)
