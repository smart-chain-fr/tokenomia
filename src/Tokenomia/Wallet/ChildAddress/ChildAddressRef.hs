{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tokenomia.Wallet.ChildAddress.ChildAddressRef (
  ChildAddressIndex (..),
  ChildAddressRef (..),
  IndexedAddress (..),
  CollateralAddressRef (..),
  FeeAddressRef (..),
) where

import Tokenomia.Common.Address (Address (..))
import Tokenomia.Wallet.Type (WalletName)

newtype ChildAddressIndex = ChildAddressIndex Integer
  deriving newtype (Eq, Ord, Num, Real, Integral, Enum)
  deriving stock (Read, Show)

data ChildAddressRef = ChildAddressRef {name :: WalletName, index :: ChildAddressIndex} deriving stock (Eq, Show)

instance Ord ChildAddressRef where
  compare ChildAddressRef {index = x} ChildAddressRef {index = y} = compare x y

data IndexedAddress = IndexedAddress
  { address :: Address
  , childAddressRef :: ChildAddressRef
  }
  deriving stock (Eq)

instance Show IndexedAddress where
  show
    IndexedAddress
      { address = Address address
      , childAddressRef = ChildAddressRef {index = ChildAddressIndex index, name = name}
      } =
      name <> " | " <> show index <> " | " <> address

newtype CollateralAddressRef = CollateralAddressRef ChildAddressRef
newtype FeeAddressRef = FeeAddressRef ChildAddressRef
