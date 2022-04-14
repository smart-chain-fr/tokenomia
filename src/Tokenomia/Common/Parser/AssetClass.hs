{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tokenomia.Common.Parser.AssetClass
    ( assetClassParser
    ) where

import Tokenomia.Common.Data.Convertible
    ( Convertible(convert) )

import Prelude           hiding ( take )

import Control.Applicative      ( (<|>) )
import Data.Char                ( isSpace )
import Data.Attoparsec.Text     ( Parser, take, takeWhile1 )

import Ledger.Ada               ( adaSymbol, adaToken )
import Ledger.Value
    ( AssetClass
    , CurrencySymbol
    , TokenName
    , assetClass
    , currencySymbol
    , tokenName
    )


currencySymbolParser :: Parser CurrencySymbol
currencySymbolParser = currencySymbol . convert <$> take 56

tokenNameParser :: Parser TokenName
tokenNameParser = tokenName . convert <$> takeWhile1 (not . isSpace)

assetClassParser :: Parser AssetClass
assetClassParser =
    adaAssetClass <|> tokenAssetClass <|> anonymousAssetClass
  where
    adaAssetClass :: Parser AssetClass
    adaAssetClass = assetClass adaSymbol <$> (adaToken <$ "lovelace")

    tokenAssetClass :: Parser AssetClass
    tokenAssetClass = assetClass
        <$> currencySymbolParser
        <*  "."
        <*> tokenNameParser

    anonymousAssetClass :: Parser AssetClass
    anonymousAssetClass = assetClass 
        <$> currencySymbolParser
        <*> pure (tokenName "")