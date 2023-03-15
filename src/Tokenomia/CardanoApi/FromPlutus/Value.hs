{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE TupleSections                             #-}

module Tokenomia.CardanoApi.FromPlutus.Value
    ( assetClassAsAssetId
    , currencySymbolAsPolicyId
    , fromPlutusValue
    , tokenNameAsAssetName
    ) where

import Data.Either.Combinators                         ( maybeToRight )
import Data.Functor                                    ( (<&>) )
import Tokenomia.Common.Data.Either.Extra              ( toEither )

import Cardano.Api
    ( AsType(..)
    , AssetId(..)
    , AssetName
    , PolicyId
    , Quantity(..)
    , Value
    , deserialiseFromRawBytes
    , valueFromList
    )

import Plutus.V1.Ledger.Value
    ( AssetClass(..)
    , CurrencySymbol(..)
    , TokenName(..)
    , adaSymbol
    , adaToken
    , assetClass
    , flattenValue
    )
import Plutus.V1.Ledger.Value qualified
    as Plutus                                          ( Value )
import PlutusTx.Builtins                               ( fromBuiltin )

import Tokenomia.CardanoApi.FromPlutus.Error           ( FromPlutusError(..) )


-- | Convert a CurrencySymbol to a PolicyId
currencySymbolAsPolicyId :: CurrencySymbol -> Either FromPlutusError PolicyId
currencySymbolAsPolicyId (CurrencySymbol x) =
    maybeToRight PlutusCurrencySymbolNotPolicyId $
        deserialiseFromRawBytes AsPolicyId (fromBuiltin x)

-- | Convert a TokenName to an AssetName
tokenNameAsAssetName :: TokenName -> Either FromPlutusError AssetName
tokenNameAsAssetName (TokenName x) =
    maybeToRight PlutusTokenNameNotAssetName $
        deserialiseFromRawBytes AsAssetName (fromBuiltin x)

-- | Convert an AssetClass to an AssetId
assetClassAsAssetId :: AssetClass -> Either FromPlutusError AssetId
assetClassAsAssetId (AssetClass (cs, tn))
    | cs == adaSymbol =
        toEither (tn == adaToken)
            PlutusAssetClassNotAssetId
            AdaAssetId
    | otherwise =
            AssetId
                <$> currencySymbolAsPolicyId cs
                <*> tokenNameAsAssetName tn

-- | Convert a Plutus Value to a Cardano.Api Value
fromPlutusValue :: Plutus.Value -> Either FromPlutusError Value
fromPlutusValue value =
    valueFromList <$> mapM fromPlutusSingleton (flattenValue value)
  where
    fromPlutusSingleton (cs, tn, x) =
        assetClassAsAssetId (assetClass cs tn) <&> (, Quantity x)
