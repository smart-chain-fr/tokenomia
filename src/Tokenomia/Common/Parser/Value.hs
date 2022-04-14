{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tokenomia.Common.Parser.Value
    ( allValuesParser
    , valueParser
    ) where

import Tokenomia.Common.Parser.AssetClass (assetClassParser)

import Data.Attoparsec.Text
    ( Parser
    , decimal
    , sepBy1
    , skipSpace
    , signed
    )

import Prelude           hiding ( take )
import Data.List.NonEmpty       ( NonEmpty, fromList )

import Plutus.V1.Ledger.Api     ( Value )
import Ledger.Value             ( assetClassValue )


valueParser :: Parser Value
valueParser = flip assetClassValue
    <$> signed decimal
    <*  skipSpace
    <*> assetClassParser

allValuesParser :: Parser (NonEmpty Value)
allValuesParser = fromList <$> valueParser `sepBy1` " + "