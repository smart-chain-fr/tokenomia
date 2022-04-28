{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tokenomia.Common.Parser.TxOutDatumHash
    ( txOutDatumHash
    ) where

import Tokenomia.Common.Data.Convertible
    ( Convertible(convert) )

import Prelude           hiding ( take )
import Control.Applicative      ( (<|>) )
import Control.Monad            ( void )
import Data.Attoparsec.Text     ( Parser, take)

import Plutus.V1.Ledger.Api     ( DatumHash(DatumHash) )


txOutDatumHash :: Parser (Maybe DatumHash)
txOutDatumHash =
        (Nothing <$  datumHashNone)
    <|> (Just    <$> datumHash)
  where
    datumHashNone :: Parser ()
    datumHashNone = void ("TxOutDatumHashNone" <|> "TxOutDatumNone")

    datumHash :: Parser DatumHash
    datumHash = DatumHash . convert
        <$> ("TxOutDatumHash ScriptDataInAlonzoEra " *> quote (take 56))

    quote :: Parser a -> Parser a
    quote p = "\"" *> p <* "\""