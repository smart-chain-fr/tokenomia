{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tokenomia.Common.Parser.AssetClass
    ( assetClass
    ) where

import Tokenomia.Common.Data.Convertible
    ( Convertible(convert) )

import Prelude           hiding ( take )

import Control.Applicative      ( (<|>) )
import Data.Attoparsec.Text     ( Parser, take, takeWhile1 )
import Data.Char                ( isSpace )
import Data.String              ( fromString )

import Ledger.Ada               ( adaSymbol, adaToken )
import Ledger.Value
    ( AssetClass
    , CurrencySymbol
    , TokenName
    )
import Ledger.Value qualified as Ledger
    ( assetClass )


currencySymbol :: Parser CurrencySymbol
currencySymbol = fromString . convert <$> take 56

tokenName :: Parser TokenName
tokenName = fromString . convert <$> takeWhile1 (not . isSpace)

assetClass :: Parser AssetClass
assetClass =
    adaAssetClass <|> tokenAssetClass <|> anonymousAssetClass
  where
    adaAssetClass :: Parser AssetClass
    adaAssetClass = Ledger.assetClass adaSymbol <$> (adaToken <$ "lovelace")

    tokenAssetClass :: Parser AssetClass
    tokenAssetClass = Ledger.assetClass
        <$> currencySymbol
        <*  "."
        <*> tokenName

    anonymousAssetClass :: Parser AssetClass
    anonymousAssetClass = Ledger.assetClass
        <$> currencySymbol
        <*> pure (fromString "")