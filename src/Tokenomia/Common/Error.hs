{-# LANGUAGE DerivingStrategies                        #-}
{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE LambdaCase                                #-}

module Tokenomia.Common.Error
    ( TokenomiaError(..)
    , whenLeftThrow
    , whenNothingThrow
    , whenNullThrow
    , whenSomethingThrow
    ) where


import Blockfrost.Client qualified as B
import Blockfrost.Types                                ( TxHash )
import Control.Monad.Except                            ( MonadError(throwError) )
import Data.List.NonEmpty                              ( NonEmpty, nonEmpty )
import Ledger.Value                                    ( Value )
import Tokenomia.Common.Address                        ( Address(..) )
import Tokenomia.Wallet.Type                           ( WalletName )

data TokenomiaError
    = NoWalletRegistered
    | NoWalletWithoutCollateral
    | NoWalletWithCollateral
    | WalletWithoutCollateral
    | AlreadyACollateral
    | NoADAsOnChildAddress
    | NoUTxOWithOnlyOneToken
    | TryingToBurnTokenWithoutScriptRegistered
    | BlockFrostError B.BlockfrostError
    | NoActiveAddressesOnWallet
    | ChildAddressNotIndexed WalletName Address
    | InconsistenciesBlockFrostVSLocalNode String
    | NoDerivedChildAddress
    | NoUTxOsFound
    | InvalidTransaction String
    | SendingsContainsZeroValue
    | SendingsNoSuchTransactions [TxHash]
    | SendingsJSONDecodingFailure String
    | SendingsValueMismatch (Value, Value)
    | MalformedAddress
    | InvalidPrivateSale String
    | QueryFailure String
    deriving stock Show

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
