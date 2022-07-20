{-# LANGUAGE LambdaCase #-}

module Tokenomia.Common.Error
    ( TokenomiaError (..)
    , whenNullThrow
    , whenSomethingThrow
    , whenNothingThrow
    , whenLeftThrow) where


import           Control.Monad.Except
import           Data.List.NonEmpty (nonEmpty, NonEmpty)
import           Tokenomia.Common.Address ( Address(..) )
import qualified Blockfrost.Client as B
import           Tokenomia.Wallet.Type
import           Blockfrost.Types (TxHash)
import           Ledger.Value (Value)

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
    | SendingsContainsZeroValue
    | SendingsNoSuchTransactions [TxHash]
    | SendingsJSONDecodingFailure String
    | SendingsValueMismatch (Value, Value)
    | SendingsMalformedAddress
    deriving Show

whenNullThrow :: MonadError e m => e -> [a]  -> m (NonEmpty a)
whenNullThrow err =
    (\case
      Nothing -> throwError err
      Just xs -> return xs) . nonEmpty

whenSomethingThrow :: MonadError e m => (a -> e) -> Maybe a  -> m ()
whenSomethingThrow toErr = maybe (pure ()) (throwError . toErr)

whenNothingThrow :: MonadError e m => e -> Maybe a ->  m a
whenNothingThrow err = maybe (throwError err) pure

whenLeftThrow :: MonadError e m => (a -> e) -> Either a b ->  m b
whenLeftThrow toErr = either (throwError . toErr) pure

