{-# LANGUAGE OverloadedStrings                         #-}

module Tokenomia.Common.Parser.TxOutDatumHash
    ( txOutDatumHash
    ) where

import Tokenomia.Common.Data.Convertible               ( convert )

import Prelude hiding                                  ( takeWhile )

import Control.Applicative                             ( (<|>) )
import Control.Monad                                   ( void )

import Data.Attoparsec.Text                            ( Parser, takeWhile )
import Data.String                                     ( fromString )
import Data.Text                                       ( Text )

import Plutus.V1.Ledger.Api                            ( DatumHash )


txOutDatumHash :: Parser (Maybe DatumHash)
txOutDatumHash =
        (Nothing <$  datumHashNone)
    <|> (Just    <$> datumHash)
  where
    datumHashNone :: Parser ()
    datumHashNone = void ("TxOutDatumHashNone" <|> "TxOutDatumNone")

    datumHash :: Parser DatumHash
    datumHash = fromString . convert
        <$> ("TxOutDatumHash ScriptDataInAlonzoEra " *> quoted)

    quoted :: Parser Text
    quoted = quote $ takeWhile (/= '\"')

    quote :: Parser a -> Parser a
    quote p = "\"" *> p <* "\""
