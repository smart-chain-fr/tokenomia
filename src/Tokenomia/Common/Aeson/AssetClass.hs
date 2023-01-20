{-# LANGUAGE OverloadedStrings              #-}

module Tokenomia.Common.Aeson.AssetClass
    ( assetClassFromJSON
    , assetClassToJSON
    ) where

import Control.Arrow                        ( (***) )

import Data.Aeson.Types                     ( Parser )
import Data.Aeson                           ( Value, object, withObject, (.:), (.=) )
import Data.String                          ( IsString(..) )

import Plutus.V1.Ledger.Value               ( AssetClass(..), assetClass, toString )


-- | Alternative AssetClass Parser to the FromJSON instance
assetClassFromJSON :: Value -> Parser AssetClass
assetClassFromJSON = withObject "AssetClass" $ \o ->
    assetClass
        <$> (fromString <$> o .: "currencySymbol")
        <*> (fromString <$> o .: "tokenName")

-- | Alternative AssetClass Value to the ToJSON instance
assetClassToJSON :: AssetClass -> Value
assetClassToJSON x =
    let (currencySymbol, tokenName) = (show *** toString) $ unAssetClass x
    in
        object
            [ "currencySymbol" .= currencySymbol
            , "tokenName" .= tokenName
            ]
