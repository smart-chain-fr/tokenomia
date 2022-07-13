{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Tokenomia.Wallet.WalletUTxO (
  WalletUTxO (..),
  getAdas,
  value,
  getDatumHashesAndAdaStrict,
) where

import Tokenomia.Common.Shell.InteractiveMenu (
  DisplayMenuItem (..),
 )

import Control.Monad.Except
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NEL
import Data.Maybe
import Ledger.Ada
import Ledger.Value (Value)
import Tokenomia.Common.Error
import Tokenomia.Common.Hash
import Tokenomia.Common.TxOutRef (showTxOutRef)
import Tokenomia.Common.Value
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
import Tokenomia.Wallet.Type ()
import Tokenomia.Wallet.UTxO hiding (value)
import Tokenomia.Wallet.UTxO qualified as UTxO (value)
import Prelude as P

data WalletUTxO = WalletUTxO
  { childAddressRef :: ChildAddressRef
  , utxo :: UTxO
  }
  deriving stock (Eq)

value :: WalletUTxO -> Value
value = UTxO.value . utxo

getAdas :: WalletUTxO -> Ada
getAdas = fromValue . value

getDatumHashesAndAdaStrict ::
  (MonadError TokenomiaError m) =>
  NEL.NonEmpty WalletUTxO ->
  m (NEL.NonEmpty (Hash, Ada, WalletUTxO))
getDatumHashesAndAdaStrict xs =
  whenNothingThrow
    ICOExchangeUtxoWithoutHash
    (NEL.nonEmpty . catMaybes . NEL.toList $ getDatumHashAndAdaMaybe <$> xs)
    >>= ( \case
            ys | NEL.length ys /= NEL.length xs -> throwError ICOExchangeUtxoWithoutHash
            ys -> return ys
        )

getDatumHashAndAdaMaybe :: WalletUTxO -> Maybe (Hash, Ada, WalletUTxO)
getDatumHashAndAdaMaybe w@WalletUTxO {utxo = UTxO {maybeDatumHash = Just hash, ..}} | containingStrictlyADAs value = Just (hash, fromValue value, w)
getDatumHashAndAdaMaybe _ = Nothing

instance Ord WalletUTxO where
  compare x y = compare (utxo x) (utxo y)

-- | Intercalate non-null elements.
sepBy :: [a] -> [[a]] -> [a]
sepBy sep xs = intercalate sep $ filter (not . null) xs

showWalletUTxO :: WalletUTxO -> String
showWalletUTxO walletUTxO =
  sepBy " : " $
    [ showTxOutRef . txOutRef . utxo
    , showValueUtf8 . value
    ]
      <*> pure walletUTxO

showDatumHash :: WalletUTxO -> String
showDatumHash walletUTxO =
  maybe "" show (maybeDatumHash . utxo $ walletUTxO)

showWalletUTxOWithDatumHash :: WalletUTxO -> String
showWalletUTxOWithDatumHash walletUTxO =
  sepBy " | " $
    [ showWalletUTxO
    , showDatumHash
    ]
      <*> pure walletUTxO

instance Show WalletUTxO where
  show = showWalletUTxOWithDatumHash

instance DisplayMenuItem WalletUTxO where
  displayMenuItem = showWalletUTxO
