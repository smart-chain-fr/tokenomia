{-# LANGUAGE OverloadedStrings            #-}

module Tokenomia.TokenDistribution.Parser.AssetClass ( assetClassParser ) where

import Control.Applicative      ( liftA2 )
import Control.Monad            ( mzero )

import Data.Attoparsec.Text
    ( Parser
    , choice
    , endOfInput
    , take
    , takeText
    )

import Data.Aeson.Extras        ( tryDecode )
import Data.Text                ( Text )
import Data.Text.Encoding       ( encodeUtf8 )

import Ledger.Ada               ( adaSymbol, adaToken )
import Ledger.Value             ( AssetClass, assetClass, tokenName )

import PlutusTx.Builtins        ( toBuiltin )

import Plutus.V1.Ledger.Api
    ( BuiltinByteString
    , TokenName (..)
    , CurrencySymbol (..)
    )

import Prelude           hiding ( take )

decodeHash :: Parser Text -> Parser BuiltinByteString
decodeHash parser = parser >>= decode
  where
    decode :: Text -> Parser BuiltinByteString
    decode parsed =
        either (const mzero) (pure . toBuiltin) (tryDecode parsed)

currencySymbolParser :: Parser CurrencySymbol
currencySymbolParser = CurrencySymbol <$> decodeHash (take 56)

tokenNameParser :: Parser Text -> Parser TokenName
tokenNameParser parser = tokenName . encodeUtf8 <$> parser

assetClassParser :: Parser AssetClass
assetClassParser =
    choice [adaAssetClass, tokenAssetClass, anonymousAssetClass]
  where
    adaAssetClass :: Parser AssetClass
    adaAssetClass =
        liftA2 assetClass
            (pure adaSymbol)
            (pure adaToken <* "lovelace" <* endOfInput)

    tokenAssetClass :: Parser AssetClass
    tokenAssetClass =
        liftA2 assetClass
            (currencySymbolParser)
            (tokenNameParser ("." *> takeText))

    anonymousAssetClass :: Parser AssetClass
    anonymousAssetClass =
        liftA2 assetClass
            (currencySymbolParser)
            (TokenName "" <$ endOfInput)
