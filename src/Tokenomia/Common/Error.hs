{-# LANGUAGE LambdaCase #-}

module Tokenomia.Common.Error (
  TokenomiaError (..),
  whenNullThrow,
  whenSomethingThrow,
  whenNothingThrow,
  whenLeftThrow,
) where

import Blockfrost.Client qualified as B
import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Tokenomia.Common.Address (Address (..))
import Tokenomia.Wallet.Type

data TokenomiaError
  = NoWalletRegistered
  | NoWalletWithoutCollateral
  | NoWalletWithCollateral
  | WalletWithoutCollateral
  | AlreadyACollateral
  | NoADAsOnChildAddress
  | NoUTxOWithOnlyOneToken
  | TryingToBurnTokenWithoutScriptRegistered
  | NoVestingInProgress
  | NoFundsToBeRetrieved
  | AllFundsLocked
  | FundAlreadyRetrieved
  | BlockFrostError B.BlockfrostError
  | NoActiveAddressesOnWallet
  | ChildAddressNotIndexed WalletName Address
  | InconsistenciesBlockFrostVSLocalNode String
  | NoICOTransactionsToBePerformOnThisWallet
  | NoDerivedChildAddress
  | NoUTxOsFound
  | ICOExchangeUtxoWithoutHash
  | ICOTokensDispatchedOnMultipleUTxOs
  | ICOPaybackAddressNotAvailable String Integer
  | ICOWhitelistingNotValid Integer Integer
  | ICONoValidTxs String
  | InvalidTransaction String
  deriving stock (Show)

whenNullThrow :: MonadError e m => e -> [a] -> m (NonEmpty a)
whenNullThrow err =
  ( \case
      Nothing -> throwError err
      Just xs -> return xs
  )
    . nonEmpty

whenSomethingThrow :: MonadError e m => (a -> e) -> Maybe a -> m ()
whenSomethingThrow toErr = maybe (pure ()) (throwError . toErr)

whenNothingThrow :: MonadError e m => e -> Maybe a -> m a
whenNothingThrow err = maybe (throwError err) pure

whenLeftThrow :: MonadError e m => (a -> e) -> Either a b -> m b
whenLeftThrow toErr = either (throwError . toErr) pure
