{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Tokenomia.Vesting.PrivateSaleVerificationData (verifySendings) where

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
import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.Either.Combinators (maybeToRight)
import Data.Foldable (find)
import Data.Hex (unhex)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (fromString)
import Data.Text (pack, unpack)
import Distribution.Simple.Utils (safeHead)
import GHC.Generics (Generic)
import Ledger (POSIXTime)
import Ledger.Value (AssetClass, CurrencySymbol, TokenName, Value (getValue), assetClass, flattenValue)
import Money qualified
import Tokenomia.Common.Blockfrost (projectFromEnv'')
import Tokenomia.Common.Environment (Environment)
import Tokenomia.Common.Error (TokenomiaError (BlockFrostError))

--TODO: getting 'no instance for Aeson.FromJSONKey TxHash' for PrivateSale type below
--TODO: this module is simply validation ... so it should take a PrivateSale and parse it as Sendings, then do validation?

data Sendings = Sendings
  { sendingsRecipientAddress :: Address
  , sendingsTxValues :: Map TxHash Value
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data PrivateSale = PrivateSale
  { psAddress :: Address
  , psInvestments :: Map TxHash Value
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

verifySendings ::
  forall (m :: Type -> Type).
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  m ()
verifySendings = do
  liftIO . putStrLn $ "Please enter a filepath with JSON data"
  jsonFilePath <- liftIO getLine
  verifySendings' jsonFilePath

verifySendings' ::
  forall (m :: Type -> Type).
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  FilePath ->
  m ()
verifySendings' jsonFilePath = do
  ps <- jsonToPrivateSale jsonFilePath
  --some intermediate step that converts the PrivateSale type to our simplified type, named sendings
  treasAddrTxs <- getTreasAddrTxs sendings -- pulls addrTxhs from treasury address using blockfrost
  let treasTxhs = (^. txHash) <$> treasAddrTxs -- [TxHash]
      flatSendingsTxValues = Map.toList . sendingsTxValues $ sendings -- :: [(TxHash, Value)]
      txhs = verifyTxHashList flatSendingsTxValues treasTxhs
  if length txhs == length flatSendingsTxValues
    then verifyTxs flatSendingsTxValues txhs
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
  Sendings ->
  m [AddressTransaction]
getTreasAddrTxs sendings = do
  prj <- projectFromEnv''
  eitherErrAddrTxs <-
    liftIO $ Client.runBlockfrost prj (Client.getAddressTransactions (sendingsRecipientAddress sendings))
  liftEither $ first BlockFrostError eitherErrAddrTxs

verifyTxHashList ::
  [(TxHash, Value)] ->
  [TxHash] ->
  [TxHash]
verifyTxHashList flatSendingsTxValues treasTxhs =
  filter (`elem` treasTxhs) (fst <$> flatSendingsTxValues)

verifyTxs ::
  forall (m :: Type -> Type).
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  Sendings ->
  [TxHash] ->
  m ()
verifyTxs sendings =
  mapM_ $ verifyTx $ Map.toList $ sendingsTxValues sendings
  where
    verifyTx ::
      forall (m :: Type -> Type).
      ( MonadIO m
      , MonadError TokenomiaError m
      , MonadReader Environment m
      ) =>
      [(TxHash, Value)] ->
      TxHash ->
      m ()
    verifyTx flatTxVals txh = do
      bfTxUtxos <- getTxUtxosByTxHash txh
      txValue <- liftEither . getTxValueByTxHash $ txh flatTxVals -- :: (TxHash,Value)
      flatVal <- liftEither $ safeHeadToRight . flattenValue . snd $ txValue -- :: (CurrencySymbol, TokenName, Integer)
      let bfUtxoOutputs = bfTxUtxos ^. outputs -- :: [UtxoOutput]
          treasAddrOutputs =
            filter (\output -> output ^. address == sendingsRecipientAddress sendings) bfUtxoOutputs -- :: [UtxoOutput]
          bfAmts = concat ((^. amount) <$> treasAddrOutputs) -- :: [Amount]
          bfVals = amountToAssetValue <$> bfAmts -- [(AssetClass, Integer)]
          totalAmount = sumRelevantValues flatVal bfVals -- :: Integer
          sendingsAmount = thrd flatVal
      unless (sendingsAmount == totalAmount) $
        throwError . BlockFrostError . BlockfrostError $ "Values don't match"

thrd :: (a, b, c) -> c
thrd (_, _, z) = z

safeHeadToRight :: [a] -> Either TokenomiaError a
safeHeadToRight xs =
  maybeToRight
    ( BlockFrostError . BlockfrostError $
        "List is empty"
    )
    (safeHead xs)

flatValToAssetClass :: (CurrencySymbol, TokenName, Integer) -> AssetClass
flatValToAssetClass (cs, tn, _) = assetClass cs tn

sumRelevantValues :: (CurrencySymbol, TokenName, Integer) -> [(AssetClass, Integer)] -> Integer
sumRelevantValues flatVal = foldr (\(ac, amt) z -> if ac == flatValToAssetClass flatVal then amt + z else z) 0

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

getTxValueByTxHash ::
  TxHash ->
  [(TxHash, Value)] ->
  Either TokenomiaError (TxHash, Value)
getTxValueByTxHash txh invs =
  maybeToRight
    ( BlockFrostError . BlockfrostError $
        "Investment list doesn't contain matching TxHash"
    )
    (find (\(t, v) -> t == txh) invs)

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

-- newtype Value = Value { getValue :: Map.Map CurrencySymbol (Map.Map TokenName Integer) }
-- flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]

-- getAssetClassByInv :: (TxHash, Value) -> AssetClass
-- getAssetClassByInv inv = flattenValue . snd $ inv

-- mapToAssetClass :: Map TxHash Value -> AssetClass
-- mapToAssetClass m = flattenValue <$> (snd <$> Map.toList $ m)

-- confirmValues :: (TxHash, Value) -> [(AssetClass, Integer)] -> Integer
-- confirmValues inv = foldr (\val z -> if fst val == (,) <$> fstTriple flatValue <*> sndTriple flatValue then snd val + z else z) 0
--   where
--     --TODO: flattenValue returns a list with a tuple ... need to fold it?
--     flatValue :: [(CurrencySymbol, TokenName, Integer)]
--     flatValue = (,) <$> (fstTriple <$> (flattenValue . snd $ inv)) <*> (sndTriple <$> (flattenValue . snd $ inv))

--     foldr (\(cs,tn,i) d -> if cs == )
