{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE OverloadedStrings                         #-}

module Tokenomia.Common.Parser.Value
    ( value
    ) where

import Tokenomia.Common.Parser.AssetClass qualified
    as Parser                                          ( assetClass )

import Data.Attoparsec.Text                            ( Parser, decimal, sepBy1, skipSpace )

import Plutus.V1.Ledger.Api                            ( Value )
import Prelude hiding                                  ( take )

import Tokenomia.Common.Asset                          ( Asset(Asset), ToValue(toValue) )


value :: Parser Value
value = foldMap toValue <$> asset `sepBy1` " + "
  where
    asset :: Parser Asset
    asset = flip Asset
        <$> decimal
        <*  skipSpace
        <*> Parser.assetClass
