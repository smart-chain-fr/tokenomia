{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tokenomia.Common.Parser.DatumHash
    ( datumHashParser
    ) where

import Tokenomia.Common.Data.Convertible
    ( Convertible(convert) )

import Prelude           hiding ( take )
import Control.Applicative      ( (<|>) )
import Control.Monad            ( void )
import Data.Attoparsec.Text     ( Parser, take)

import Plutus.V1.Ledger.Api     ( DatumHash(DatumHash) )


datumHashParser :: Parser (Maybe DatumHash)
datumHashParser =
        (Nothing <$  datumHashNone)
    <|> (Just    <$> datumHash)
  where
    datumHashNone :: Parser ()
    datumHashNone = void ("TxOutDatumHashNone" <|> "TxOutDatumNone")

    datumHash :: Parser DatumHash
    datumHash = DatumHash . convert
        <$> ("TxOutDatumHash ScriptDataInAlonzoEra " *> quoteParser(take 56))

    quoteParser :: Parser a -> Parser a
    quoteParser p = "\"" *> p <* "\""