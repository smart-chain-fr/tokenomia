module Tokenomia.TokenDistribution.Wallet.ChildAddress.ChildAddressRef (
  maxChildAddressIndexRequired,
  defaultFeeAddressRef,
  defaultCollateralAddressRef,
) where

import Data.Composition ((.:))
import Data.List.NonEmpty (NonEmpty)

import Tokenomia.TokenDistribution.Distribution (Distribution)
import Tokenomia.Wallet.Type (WalletName)

import Tokenomia.Wallet.ChildAddress.ChildAddressRef (
  ChildAddressIndex (..),
  ChildAddressRef (..),
  CollateralAddressRef (..),
  FeeAddressRef (..),
 )

maxChildAddressIndexRequired :: NonEmpty Distribution -> ChildAddressIndex
maxChildAddressIndexRequired = ChildAddressIndex . fromIntegral . length

feeAddressRefAtIndex :: WalletName -> ChildAddressIndex -> FeeAddressRef
feeAddressRefAtIndex = FeeAddressRef .: ChildAddressRef

collateralAddressRefAtIndex :: WalletName -> ChildAddressIndex -> CollateralAddressRef
collateralAddressRefAtIndex = CollateralAddressRef .: ChildAddressRef

defaultFeeAddressRef :: WalletName -> FeeAddressRef
defaultFeeAddressRef = flip feeAddressRefAtIndex 0

defaultCollateralAddressRef :: WalletName -> CollateralAddressRef
defaultCollateralAddressRef = flip collateralAddressRefAtIndex 0
