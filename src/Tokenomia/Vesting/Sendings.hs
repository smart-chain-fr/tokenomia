{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Tokenomia.Vesting.Sendings (Sendings (Sendings), MonadRunBlockfrost (getAddressTransactions, getTxUtxos), jsonToSendings, verifySendings, verifySendings') where

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
import Control.Monad.Identity (IdentityT (IdentityT))
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, eitherDecode)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as ByteString
import Data.Default (def)
import Data.Either.Combinators (maybeToRight)
import Data.Foldable (find)
import Data.Hex (unhex)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (fromString)
import Data.Text (pack, unpack)
import Data.Tuple.Extra (thd3)
import Distribution.Simple.Utils (safeHead)
import GHC.Generics (Generic)
import Ledger.Value (AssetClass, CurrencySymbol, TokenName, Value, assetClass, flattenValue)
import Money qualified
import Tokenomia.Common.Blockfrost (projectFromEnv'')
import Tokenomia.Common.Environment (Environment)
import Tokenomia.Common.Error (TokenomiaError (BlockFrostError))

data Sendings = Sendings
    { sendingsRecipientAddress :: Address
  , sendingsTxValues :: Map TxHash Value
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

deriving stock instance Ord TxHash
deriving newtype instance ToJSONKey TxHash
deriving newtype instance FromJSONKey TxHash

class Monad m => MonadRunBlockfrost m where
  getAddressTransactions :: Address -> m [AddressTransaction]
  getTxUtxos :: TxHash -> m TransactionUtxos

newtype RealBlockfrost (m :: Type -> Type) (a :: Type) = RealBlockfrost {runRealBlockfrost :: m a}
  deriving (Functor, Applicative, Monad) via IdentityT m

deriving via (IdentityT m) instance (MonadReader r m) => MonadReader r (RealBlockfrost m)
deriving via (IdentityT m) instance (MonadError e m) => MonadError e (RealBlockfrost m)

-- Starting page number is 1
getAddressTransactionsPaged :: Address -> Int -> Client.BlockfrostClient [AddressTransaction]
getAddressTransactionsPaged ad pageNo = Client.getAddressTransactions' ad (Client.Paged 100 pageNo) def Nothing Nothing

getAllAddressTransactions :: Address -> Client.BlockfrostClient [AddressTransaction]
getAllAddressTransactions ad = concat <$> sequenceWhile (not . null) (getAddressTransactionsPaged ad <$> [1 ..])

-- Takes IO actions and calls them until the return value does not satisfy the predicate. Does not include the result of the first failing action.
sequenceWhile :: forall (m :: Type -> Type) (a :: Type). Monad m => (a -> Bool) -> [m a] -> m [a]
sequenceWhile _ [] = pure []
sequenceWhile p (m : ms) = do
  a <- m
  if p a
    then (a :) <$> sequenceWhile p ms
    else pure []

instance (MonadIO m, MonadReader Environment m, MonadError TokenomiaError m) => MonadRunBlockfrost (RealBlockfrost m) where
  getAddressTransactions ad = RealBlockfrost $ do
    prj <- projectFromEnv''
    eitherErrAddrTxs <- liftIO $ Client.runBlockfrost prj (getAllAddressTransactions ad)
    liftEither $ first BlockFrostError eitherErrAddrTxs
  getTxUtxos txh = RealBlockfrost $ do
    prj <- projectFromEnv''
    eitherErrTxUtxos <- liftIO $ Client.runBlockfrost prj (Client.getTxUtxos txh)
    liftEither $ first BlockFrostError eitherErrTxUtxos

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
  jsonContents <- liftIO $ ByteString.readFile jsonFilePath
  sendings <- jsonToSendings jsonContents
  runRealBlockfrost $ verifySendings' sendings

verifySendings' ::
  forall (m :: Type -> Type).
  ( MonadRunBlockfrost m
  , MonadError TokenomiaError m
  ) =>
  Sendings ->
  m ()
verifySendings' sendings = do
  -- TODO: maybe check if sendins is empty early to prevent unnecessary calls to Blockfrost
  treasAddrTxs <- getAddressTransactions (sendingsRecipientAddress sendings)
  let treasTxhs = (^. txHash) <$> treasAddrTxs
      flatSendingsTxValues = Map.toList . sendingsTxValues $ sendings
      txhs = verifyTxHashList flatSendingsTxValues treasTxhs
  if length txhs == length flatSendingsTxValues
    then verifyTxs sendings txhs -- this can be refactored to be called with 'sendings' only, as the above line ensures that all tx hashes in `sendings` are equivalent to tx hashes in `txhs`
    else
      throwError . BlockFrostError . BlockfrostError $
        "Missing Transactions"

jsonToSendings ::
  forall (m :: Type -> Type).
  ( MonadError TokenomiaError m
  ) =>
  ByteString.ByteString ->
  m Sendings
jsonToSendings jsonByteString = do
  let eitherErrSendings = eitherDecode jsonByteString
  liftEither $ first (BlockFrostError . BlockfrostError . pack) eitherErrSendings

verifyTxHashList ::
  [(TxHash, Value)] ->
  [TxHash] ->
  [TxHash]
verifyTxHashList flatSendingsTxValues treasTxhs =
  filter (`elem` treasTxhs) (fst <$> flatSendingsTxValues)

verifyTxs ::
  forall (m :: Type -> Type).
  ( MonadRunBlockfrost m
  , MonadError TokenomiaError m
  ) =>
  Sendings ->
  [TxHash] ->
  m ()
verifyTxs sendings =
  mapM_ $ verifyTx $ Map.toList $ sendingsTxValues sendings
  where
    verifyTx ::
      forall (m :: Type -> Type).
      ( MonadRunBlockfrost m
      , MonadError TokenomiaError m
      ) =>
      [(TxHash, Value)] ->
      TxHash ->
      m ()
    verifyTx flatTxVals txh = do
      txValue <- liftEither (getTxValueByTxHash txh flatTxVals)
      flatVal <- liftEither $ safeHeadToRight . flattenValue . snd $ txValue
      bfTxUtxos <- getTxUtxos txh
      let bfUtxoOutputs = bfTxUtxos ^. outputs
          treasAddrOutputs =
            filter (\output -> output ^. address == sendingsRecipientAddress sendings) bfUtxoOutputs
          bfAmts = concat ((^. amount) <$> treasAddrOutputs)
          bfVals = amountToAssetValue <$> bfAmts
          totalAmount = sumRelevantValues flatVal bfVals
          sendingsAmount = thd3 flatVal
      unless (sendingsAmount == totalAmount) $
        throwError . BlockFrostError . BlockfrostError $ "Values don't match "

-- Alternative simpler implementation. Assumes Sendings has been validated against block frost results for included recipientAddress
verifyTxs' ::
  forall (m :: Type -> Type).
  ( MonadRunBlockfrost m
  , MonadError TokenomiaError m
  ) =>
  Sendings ->
  m ()
verifyTxs' sendings =
  mapM_ verifyTx $ Map.toList $ sendingsTxValues sendings
  where
    verifyTx ::
      forall (m :: Type -> Type).
      ( MonadRunBlockfrost m
      , MonadError TokenomiaError m
      ) =>
      (TxHash, Value) ->
      m ()
    verifyTx txValue = do
      flatVal <- liftEither $ safeHeadToRight . flattenValue . snd $ txValue
      bfTxUtxos <- getTxUtxos $ fst txValue
      let bfUtxoOutputs = bfTxUtxos ^. outputs
          treasAddrOutputs =
            filter (\output -> output ^. address == sendingsRecipientAddress sendings) bfUtxoOutputs
          bfAmts = concat ((^. amount) <$> treasAddrOutputs)
          bfVals = amountToAssetValue <$> bfAmts
          totalAmount = sumRelevantValues flatVal bfVals
          sendingsAmount = thd3 flatVal
      unless (sendingsAmount == totalAmount) $
        throwError . BlockFrostError . BlockfrostError $ "Values don't match "

-- This can't happen since if tx hash list is empty verifyTx in mapM_ will not
-- be called
safeHeadToRight :: forall (a :: Type). [a] -> Either TokenomiaError a
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

getTxValueByTxHash ::
  TxHash ->
  [(TxHash, Value)] ->
  Either TokenomiaError (TxHash, Value)
getTxValueByTxHash txh invs =
  maybeToRight
    ( BlockFrostError . BlockfrostError $
        "Investment list doesn't contain matching TxHash"
    )
    (find (\(t, _) -> t == txh) invs)

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
