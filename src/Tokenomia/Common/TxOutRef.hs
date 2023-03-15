{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE RecordWildCards                           #-}
{-# OPTIONS_GHC -Wno-orphans                           #-}
module Tokenomia.Common.TxOutRef
    ( TxOutRef(..)
    , showTxOutRef
    ) where

import Data.Text qualified as T

import Ledger                                          ( TxOutRef(..) )

import Tokenomia.Common.Serialise                      ( ToCLI(..) )


instance ToCLI TxOutRef where
  toCLI = T.pack . showTxOutRef

showTxOutRef :: TxOutRef -> String
showTxOutRef TxOutRef {..} = show txOutRefId <> "#" <> show txOutRefIdx
