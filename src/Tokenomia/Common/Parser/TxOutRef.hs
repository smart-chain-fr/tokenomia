module Tokenomia.Common.Parser.TxOutRef
    ( txOutRef
    ) where

import Tokenomia.Common.Data.Convertible
    ( convert )

import Data.Attoparsec.Text
    ( Parser
    , decimal
    , skipSpace
    , takeWhile1
    )

import Prelude           hiding ( take )
import Data.Char                ( isSpace )
import Data.String              ( fromString )

import Ledger                   ( TxOutRef(TxOutRef), TxId )


txOutRef :: Parser TxOutRef
txOutRef = TxOutRef
    <$> txOutRefId
    <*  skipSpace
    <*> txOutRefIx
  where
    txOutRefId :: Parser TxId
    txOutRefId = fromString . convert <$> takeWhile1 (not . isSpace)

    txOutRefIx :: Parser Integer
    txOutRefIx = decimal
