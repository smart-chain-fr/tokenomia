{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tokenomia.Wallet.ChildAddress.ChildAddressRef 
    ( ChildAddressIndex (..)
    , ChildAddressRef (..)
    , IndexedAddress (..)
    , CollateralAddressRef (..)
    , FeeAddressRef (..)) where

import Tokenomia.Wallet.Type
import Tokenomia.Common.Address ( Address(..) )

newtype ChildAddressIndex = ChildAddressIndex Integer deriving (Eq,Ord,Num,Enum,Read,Show)

data ChildAddressRef = ChildAddressRef {name :: WalletName, index :: ChildAddressIndex } deriving (Eq,Show)

instance Ord ChildAddressRef where
    compare ChildAddressRef {index = x} ChildAddressRef {index = y} = compare x y


data IndexedAddress
        = IndexedAddress
          { address         :: Address
          , childAddressRef :: ChildAddressRef} deriving (Eq,Show)

newtype CollateralAddressRef = CollateralAddressRef ChildAddressRef
newtype FeeAddressRef = FeeAddressRef ChildAddressRef 


