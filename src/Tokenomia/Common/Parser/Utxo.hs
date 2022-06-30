{-# LANGUAGE OverloadedStrings          #-}

module Tokenomia.Common.Parser.Utxo
    ( utxo
    ) where

import Data.Attoparsec.Text
    ( Parser
    , skipSpace
    )

import Plutus.V1.Ledger.Api
    ( DatumHash
    , TxOutRef
    , Value
    )

import Tokenomia.Common.Parser.TxOutRef         ( txOutRef )
import Tokenomia.Common.Parser.Value            ( value )
import Tokenomia.Common.Parser.TxOutDatumHash   ( txOutDatumHash )


utxo :: Parser (TxOutRef, Value, Maybe DatumHash)
utxo = (,,)
    <$> txOutRef
    <*  skipSpace
    <*> value
    <*  " + "
    <*> txOutDatumHash
