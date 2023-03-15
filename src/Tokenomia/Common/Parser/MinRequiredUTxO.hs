{-# LANGUAGE OverloadedStrings                         #-}

module Tokenomia.Common.Parser.MinRequiredUTxO
    ( minRequiredUTxO
    ) where

import Data.Attoparsec.Text                            ( Parser, decimal, skipSpace )

import Ledger.Ada                                      ( lovelaceValueOf )
import Ledger.Value                                    ( Value )


minRequiredUTxO :: Parser Value
minRequiredUTxO =
       "Lovelace"
     *> skipSpace
     *> fmap lovelaceValueOf decimal
