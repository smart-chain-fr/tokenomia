module Tokenomia.Common.Parser.TxOutRef (
  txOutRef,
) where

import Tokenomia.Common.Data.Convertible (
  convert,
 )

import Data.Attoparsec.Text (
  Parser,
  decimal,
  skipSpace,
  takeWhile1,
 )

import Data.Char (isSpace)
import Data.String (fromString)
import Prelude hiding (take)

import Ledger (TxOutRef (TxOutRef))
import Ledger.TxId (TxId)

txOutRef :: Parser TxOutRef
txOutRef =
  TxOutRef
    <$> txOutRefId
    <* skipSpace
    <*> txOutRefIx
  where
    txOutRefId :: Parser TxId
    txOutRefId = fromString . convert <$> takeWhile1 (not . isSpace)

    txOutRefIx :: Parser Integer
    txOutRefIx = decimal
