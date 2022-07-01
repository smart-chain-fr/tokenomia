{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tokenomia.Vesting.PrivateSale (verifyPrivateSale) where

import Blockfrost.Client (
  Address,
  AddressTransaction,
  Amount (AdaAmount, AssetAmount),
  BlockfrostError (BlockfrostError),
  TransactionUtxos,
  TxHash,
  runBlockfrost,
 )
import Blockfrost.Client qualified as Client
import Blockfrost.Lens (address, amount, outputs, txHash)
import Control.Lens (folded, makeLenses, toListOf, (^.))
import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Identity (IdentityT (IdentityT))
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Bifunctor (first)
import Data.Either.Combinators (maybeToRight)
import Data.Foldable (find)
import Data.Hex (unhex)
import Data.Kind (Type)
import Data.String (fromString)
import Data.Text (pack, unpack)
import GHC.Generics (Generic)
import Ledger (POSIXTime)
import Ledger.Value (AssetClass, assetClass)
import Money qualified
import Tokenomia.Common.Blockfrost (projectFromEnv'')
import Tokenomia.Common.Environment (Environment)
import Tokenomia.Common.Error (TokenomiaError (BlockFrostError))

data PrivateSale = PrivateSale
  { -- | Treasury address
    _psAddress :: Address
  , -- | Starting time of private sale
    _psStart :: POSIXTime
  , -- | List of tranches containing percentages and durations
    _psTranches :: [Tranche]
  , -- | Vesting token
    _psAssetClass :: AssetClass
  , -- | All investors
    _psInvestors :: [PrivateInvestor]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data Tranche = Tranche
  { _tranchePercentage :: Integer
  , _trancheDuration :: Integer
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data PrivateInvestor = PrivateInvestor
  { _piAddress :: Address
  , -- | Amount of vesting tokens to lock (note, this is likely NOT equal
    -- to the total amounts of investments, as different tokens)
    _piAllocation :: Integer
  , _piInvestments :: [Investment]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data Investment = Investment
  { -- | Tx that sends asset class to payment address
    _invTx :: TxHash
  , -- | Asset class to be sent to payment address
    _invAssetClass :: AssetClass
  , -- | Amount of asset class expected to be sent to payment address
    _invAmount :: Integer
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

makeLenses ''PrivateSale
makeLenses ''Tranche
makeLenses ''PrivateInvestor
makeLenses ''Investment

class Monad m => MonadRunBlockfrost m where
  getAddressTransactions :: Address -> m [AddressTransaction]
  getTxUtxos :: TxHash -> m TransactionUtxos

newtype RealBlockfrost (m :: Type -> Type) (a :: Type) = RealBlockfrost {runRealBlockfrost :: m a}
  deriving (Functor, Applicative, Monad) via IdentityT m

newtype FakeBlockfrost (m :: Type -> Type) (a :: Type) = FakeBlockfrost {runFakeBlockfrost :: m a}
  deriving (Functor, Applicative, Monad) via IdentityT m

-- deriving (MonadReader Environment) via IdentityT m

deriving via (IdentityT m) instance (MonadReader Environment m) => MonadReader Environment (RealBlockfrost m)
deriving via (IdentityT m) instance (MonadError TokenomiaError m) => MonadError TokenomiaError (RealBlockfrost m)

instance (MonadIO m, MonadReader Environment m, MonadError TokenomiaError m) => MonadRunBlockfrost (RealBlockfrost m) where
  getAddressTransactions ad = RealBlockfrost $ do
    prj <- projectFromEnv''
    eitherErrAddrTxs <- liftIO $ runBlockfrost prj (Client.getAddressTransactions ad)
    liftEither $ first BlockFrostError eitherErrAddrTxs
  getTxUtxos txh = RealBlockfrost $ do
    prj <- projectFromEnv''
    eitherErrTxUtxos <- liftIO $ runBlockfrost prj (Client.getTxUtxos txh)
    liftEither $ first BlockFrostError eitherErrTxUtxos

verifyPrivateSale ::
  forall (m :: Type -> Type).
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  m ()
verifyPrivateSale = do
  liftIO . putStrLn $ "Please enter a filepath with JSON data"
  jsonFilePath <- liftIO getLine
  ps <- jsonToPrivateSale jsonFilePath
  runRealBlockfrost $ verifyPrivateSale' ps

verifyPrivateSale' ::
  forall (m :: Type -> Type).
  ( MonadRunBlockfrost m
  , MonadError TokenomiaError m
  ) =>
  PrivateSale ->
  m ()
verifyPrivateSale' ps = do
  treasAddrTxs <- getAddressTransactions (ps ^. psAddress)
  let invTxhs = getTxhsByPrivateSale ps
      txhs = verifyTxHashList invTxhs treasAddrTxs
  if length txhs == length invTxhs
    then verifyTxs ps txhs
    else
      throwError . BlockFrostError . BlockfrostError $
        "Missing Transactions"

jsonToPrivateSale ::
  forall (m :: Type -> Type).
  ( MonadIO m
  , MonadError TokenomiaError m
  ) =>
  FilePath ->
  m PrivateSale
jsonToPrivateSale jsonFilePath = do
  eitherErrPs <- liftIO . eitherDecodeFileStrict $ jsonFilePath
  liftEither $ first (BlockFrostError . BlockfrostError . pack) eitherErrPs

getTxhsByPrivateSale :: PrivateSale -> [TxHash]
getTxhsByPrivateSale ps = (^. invTx) <$> getPsInvestments ps

verifyTxs ::
  forall (m :: Type -> Type).
  ( MonadRunBlockfrost m
  , MonadError TokenomiaError m
  ) =>
  PrivateSale ->
  [TxHash] ->
  m ()
verifyTxs ps =
  mapM_ $ verifyTx $ getPsInvestments ps
  where
    verifyTx ::
      forall (m :: Type -> Type).
      ( MonadRunBlockfrost m
      , MonadError TokenomiaError m
      ) =>
      [Investment] ->
      TxHash ->
      m ()
    verifyTx invs txh = do
      bfTxUtxos <- getTxUtxos txh
      inv <- liftEither $ getInvByTxHash txh invs
      let bfUtxoOutputs = bfTxUtxos ^. outputs
          treasAddrOutputs =
            filter (\output -> (output ^. address) == (ps ^. psAddress)) bfUtxoOutputs
          bfAmts = concat ((^. amount) <$> treasAddrOutputs)
          bfValues = amountToAssetValue <$> bfAmts
          totalAmount = sumRelevantValues inv bfValues
          invAmount' = inv ^. invAmount
          invAssetClass' = inv ^. invAssetClass
      unless (invAmount' == totalAmount && invAssetClass' == inv ^. invAssetClass) $
        throwError . BlockFrostError . BlockfrostError $ "Values don't match"

sumRelevantValues :: Investment -> [(AssetClass, Integer)] -> Integer
sumRelevantValues inv = foldr (\(ac, amt) z -> if ac == inv ^. invAssetClass then amt + z else z) 0

getInvByTxHash ::
  TxHash ->
  [Investment] ->
  Either TokenomiaError Investment
getInvByTxHash txh invs =
  maybeToRight
    ( BlockFrostError . BlockfrostError $
        "Investment list doesn't contain matching TxHash"
    )
    (find (\inv -> (inv ^. invTx) == txh) invs)

getPsInvestments ::
  PrivateSale ->
  [Investment]
getPsInvestments = toListOf $ psInvestors . traverse . piInvestments . folded

verifyTxHashList ::
  [TxHash] ->
  [AddressTransaction] ->
  [TxHash]
verifyTxHashList txhs addrTxs =
  filter (`elem` txhs) ((^. txHash) <$> addrTxs)

{-
  Below, currencySymbolLength is the first part of a 2-part value returned by Money.someDiscreteCurrency,
  and it's 56 characters long. The String returned by Money.someDiscreteCurrency has the
  form: "CurrencysymbolName" :: Text, where we have to manually extract the currency symbol
  embedded in the first half using take, and extract the token name in the second half
  using drop.

  Also note that amountToAssetValue is partial, but the error case of it implies that blockfrost
  is broken, i.e. an unrecoverable error.
-}

amountToAssetValue :: Amount -> (AssetClass, Integer)
amountToAssetValue (AdaAmount ll) =
  (assetClass "" "", Money.someDiscreteAmount $ Money.toSomeDiscrete ll)
amountToAssetValue (AssetAmount sd) =
  (assetClass (fromString currencySymbol) (fromString name), Money.someDiscreteAmount sd)
  where
    encodedName :: String
    encodedName = unpack $ Money.someDiscreteCurrency sd

    currencySymbol :: String
    currencySymbol = take currencySymbolLength encodedName

    name :: String
    name = either error id $ unhex $ drop currencySymbolLength encodedName

    currencySymbolLength :: Int
    currencySymbolLength = 56
