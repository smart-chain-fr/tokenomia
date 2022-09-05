{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE TupleSections                  #-}

module Tokenomia.CardanoApi.FromPlutus.Value
    ( currencySymbolAsPolicyId
    , tokenNameAsAssetName
    , assetClassAsAssetId
    , fromPlutusValue
    ) where

import Data.Either.Combinators              ( maybeToRight )
import Data.Functor                         ( (<&>) )
import Tokenomia.Common.Data.Either.Extra   ( toEither )

import Cardano.Api
    ( PolicyId
    , AssetName
    , AssetId(..)
    , AsType(..)
    , Quantity(..)
    , Value
    , deserialiseFromRawBytes
    , valueFromList
    )

import PlutusTx.Builtins                    ( fromBuiltin )
import Plutus.V1.Ledger.Ada                 ( adaSymbol, adaToken )
import Plutus.V1.Ledger.Value qualified
    as Plutus                               ( Value )
import Plutus.V1.Ledger.Value
    ( AssetClass(..)
    , CurrencySymbol(..)
    , TokenName(..)
    , flattenValue
    , assetClass
    )

import Tokenomia.CardanoApi.FromPlutus.Error
    ( FromPlutusError(..) )


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
    valueFromList <$> sequence (fromPlutusSingleton <$> flattenValue value)
  where
    fromPlutusSingleton (cs, tn, x) =
        assetClassAsAssetId (assetClass cs tn) <&> (, Quantity x)
