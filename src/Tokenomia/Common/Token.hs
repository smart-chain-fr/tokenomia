{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingStrategies #-}

module Tokenomia.Common.Token
  ( Token (..)
  , getMinimumUTxOAdaRequired) where

import Plutus.V1.Ledger.Value ( AssetClass )
import Ledger.Ada

data Token
    = Token
      { assetClass :: AssetClass
      , amount     :: Integer
      , minimumAdaRequired :: Ada} deriving stock (Eq,Ord,Show)


getMinimumUTxOAdaRequired :: Ada
getMinimumUTxOAdaRequired =  1379280
