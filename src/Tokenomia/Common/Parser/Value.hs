{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tokenomia.Common.Parser.Value
    ( value
    ) where

import Tokenomia.Common.Parser.AssetClass qualified as Parser
    ( assetClass )

import Data.Attoparsec.Text
    ( Parser
    , decimal
    , sepBy1
    , skipSpace
    , signed
    )

import Prelude           hiding ( take )

import Plutus.V1.Ledger.Api     ( Value )
import Ledger.Value             ( assetClassValue )


value :: Parser Value
value = mconcat <$> singletonValue `sepBy1` " + "
  where
    singletonValue :: Parser Value
    singletonValue = flip assetClassValue
        <$> signed decimal
        <*  skipSpace
        <*> Parser.assetClass