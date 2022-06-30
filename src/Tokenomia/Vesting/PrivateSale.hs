{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Tokenomia.Vesting.PrivateSale (verifyPrivateSale) where

import Blockfrost.Client (
  Address,
  AddressTransaction,
  Amount (AdaAmount, AssetAmount),
  BlockfrostError (BlockfrostError),
  TransactionUtxos,
  TxHash,
 )
import Blockfrost.Client qualified as Client
import Blockfrost.Lens (address, amount, outputs, txHash)
import Control.Lens (makeLenses, (^.))
import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
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
  verifyPrivateSale' jsonFilePath

verifyPrivateSale' ::
  forall (m :: Type -> Type).
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  FilePath ->
  m ()
verifyPrivateSale' jsonFilePath = do
  ps <- jsonToPrivateSale jsonFilePath
  treasAddrTxs <- getTreasAddrTxs ps
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

getTreasAddrTxs ::
  forall (m :: Type -> Type).
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  PrivateSale ->
  m [AddressTransaction]
getTreasAddrTxs ps = do
  prj <- projectFromEnv''
  eitherErrAddrTxs <-
    liftIO $ Client.runBlockfrost prj (Client.getAddressTransactions (ps ^. psAddress))
  liftEither $ first BlockFrostError eitherErrAddrTxs

getTxhsByPrivateSale :: PrivateSale -> [TxHash]
getTxhsByPrivateSale ps = (^. invTx) <$> getPsInvestments ps

verifyTxs ::
  forall (m :: Type -> Type).
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  PrivateSale ->
  [TxHash] ->
  m ()
verifyTxs ps =
  mapM_ $ verifyTx $ getPsInvestments ps
  where
    verifyTx ::
      forall (m :: Type -> Type).
      ( MonadIO m
      , MonadError TokenomiaError m
      , MonadReader Environment m
      ) =>
      [Investment] ->
      TxHash ->
      m ()
    verifyTx invs txh = do
      bfTxUtxos <- getTxUtxosByTxHash txh
      inv <- liftEither $ getInvByTxHash txh invs
      let bfUtxoOutputs = bfTxUtxos ^. outputs
          treasAddrOutputs =
            filter (\output -> (output ^. address) == (ps ^. psAddress)) bfUtxoOutputs
          bfAmts = concat ((^. amount) <$> treasAddrOutputs)
          bfValues = amountToAssetValue <$> bfAmts
          confirmedVals = confirmValues inv bfValues
          invAmount' = inv ^. invAmount
          invAssetClass' = inv ^. invAssetClass
      unless (invAmount' == confirmedVals && invAssetClass' == inv ^. invAssetClass) $
        throwError . BlockFrostError . BlockfrostError $ "Values don't match"

confirmValues :: Investment -> [(AssetClass, Integer)] -> Integer
confirmValues inv = foldr (\val z -> if fst val == inv ^. invAssetClass then snd val + z else z) 0

getTxUtxosByTxHash ::
  forall (m :: Type -> Type).
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  TxHash ->
  m TransactionUtxos
getTxUtxosByTxHash txh = do
  prj <- projectFromEnv''
  eitherErrTxUtxos <- liftIO $ Client.runBlockfrost prj (Client.getTxUtxos txh)
  liftEither $ first BlockFrostError eitherErrTxUtxos

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
getPsInvestments ps = concat ((^. piInvestments) <$> investors)
  where
    investors :: [PrivateInvestor]
    investors = ps ^. psInvestors

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
