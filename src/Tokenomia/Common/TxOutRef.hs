{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Tokenomia.Common.TxOutRef 
    ( showTxOutRef
    , TxOutRef (..)
    ) where

import qualified Data.Text as T

import Ledger ( TxOutRef (..) )

import Tokenomia.Common.Serialise


instance ToCLI TxOutRef where
  toCLI = T.pack . showTxOutRef

showTxOutRef :: TxOutRef -> String
showTxOutRef TxOutRef {..} = show txOutRefId <> "#" <> show txOutRefIdx
