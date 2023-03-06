{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Tokenomia.Common.Token
  ( Token (..)
  , getMinimumUTxOAdaRequired) where

import Plutus.V1.Ledger.Value hiding (assetClass)
import Ledger.Ada

data Token
    = Token
      { assetClass :: AssetClass
      , amount     :: Integer
      , minimumAdaRequired :: Ada} deriving (Eq,Ord,Show)


getMinimumUTxOAdaRequired :: Ada
getMinimumUTxOAdaRequired =  1379280
