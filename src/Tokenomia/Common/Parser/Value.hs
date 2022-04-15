{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tokenomia.Common.Parser.Value
    ( allValues
    , value
    ) where

import Tokenomia.Common.Parser.AssetClass qualified as P
    ( assetClass )

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


value :: Parser Value
value = flip assetClassValue
    <$> signed decimal
    <*  skipSpace
    <*> P.assetClass

allValues :: Parser (NonEmpty Value)
allValues = fromList <$> value `sepBy1` " + "