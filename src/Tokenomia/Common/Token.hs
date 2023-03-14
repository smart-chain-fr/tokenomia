{-# LANGUAGE DerivingStrategies                        #-}
{-# LANGUAGE OverloadedStrings                         #-}
{-# OPTIONS_GHC -fno-warn-orphans                      #-}

module Tokenomia.Common.Token
    ( Token(..)
    , getMinimumUTxOAdaRequired
    ) where

import Ledger.Ada                                      ( Ada )
import Plutus.V1.Ledger.Value                          ( AssetClass )

data Token
    = Token
      { assetClass :: AssetClass
      , amount     :: Integer
      , minimumAdaRequired :: Ada} deriving stock (Eq,Ord,Show)


getMinimumUTxOAdaRequired :: Ada
getMinimumUTxOAdaRequired =  1379280
