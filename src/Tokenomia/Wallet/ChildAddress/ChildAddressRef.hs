{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tokenomia.Wallet.ChildAddress.ChildAddressRef 
    ( ChildAddressIndex (..)
    , ChildAddressRef (..)
    , IndexedAddress (..)) where

import Tokenomia.Wallet.Type
import Tokenomia.Common.Address ( Address(..) )

newtype ChildAddressIndex = ChildAddressIndex Integer deriving (Eq,Ord,Num,Enum,Read,Show)

data ChildAddressRef = ChildAddressRef {name :: WalletName, index :: ChildAddressIndex } deriving (Eq,Show)

data IndexedAddress
        = IndexedAddress
          { address         :: Address
          , childAddressRef :: ChildAddressRef} deriving (Eq,Show)