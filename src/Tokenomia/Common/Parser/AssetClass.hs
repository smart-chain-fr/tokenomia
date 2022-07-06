{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Tokenomia.Common.Parser.AssetClass (
  assetClass,
) where

import Prelude hiding (take)

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, take, takeWhile1)
import Data.Char (isSpace)
import Data.String (fromString)
import Data.Text (Text)

import Ledger.Value (
  AssetClass,
  CurrencySymbol,
  TokenName,
 )
import Ledger.Value qualified as Ledger (
  assetClass,
  tokenName,
 )

import Tokenomia.Common.AssetClass qualified as Ledger (
  adaAssetClass,
 )

import Tokenomia.Common.Data.ByteString (unsafeDecodeHex)
import Tokenomia.Common.Data.Convertible (Convertible (convert))

currencySymbol :: Parser CurrencySymbol
currencySymbol = fromString . convert <$> take 56

tokenName :: Parser TokenName
tokenName = decodeTokenName <$> takeWhile1 (not . isSpace)
  where
    decodeTokenName :: Text -> TokenName
    decodeTokenName = Ledger.tokenName . unsafeDecodeHex . convert

assetClass :: Parser AssetClass
assetClass =
  adaAssetClass <|> tokenAssetClass <|> anonymousAssetClass
  where
    adaAssetClass :: Parser AssetClass
    adaAssetClass = Ledger.adaAssetClass <$ "lovelace"

    tokenAssetClass :: Parser AssetClass
    tokenAssetClass =
      Ledger.assetClass
        <$> currencySymbol
        <* "."
        <*> tokenName

    anonymousAssetClass :: Parser AssetClass
    anonymousAssetClass =
      Ledger.assetClass
        <$> currencySymbol
        <*> pure (fromString "")
