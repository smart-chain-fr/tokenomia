{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Tokenomia.Vesting.PrivateSaleVerificationData () where

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
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (fromString)
import Data.Text (pack, unpack)
import GHC.Generics (Generic)
import Ledger (POSIXTime)
import Ledger.Value (AssetClass, Value(getValue), assetClass)
import Money qualified
import Tokenomia.Common.Blockfrost (projectFromEnv'')
import Tokenomia.Common.Environment (Environment)
import Tokenomia.Common.Error (TokenomiaError (BlockFrostError))

{-
removing verification logic from PrivateSale in its own reusable chunk

note: we probably won't use lenses, and after breaking up PrivateSale to
remove verification logic they shouldn't be necessary
-}

data Sendings = Sendings
   { sendingsRecipientAddress :: Address
   , sendingsTxValues  :: Map TxHash Value
   }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data PrivateSale = PrivateSale
  { psAddress :: Address
  , psInvestments :: Map TxHash Value
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

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
  treasAddrTxs <- getTreasAddrTxs ps -- pulls addrTxhs from treasury address using blockfrost
  let treasTxhs = _addressTransactionTxHash <$> treasAddrTxs -- [TxHash]
      invMap = psInvestments ps -- :: Map TxHash Value
      txhs = verifyTxHashList invMap treasTxhs
  if length txhs == length treasTxhs
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

-- getTxhsByPrivateSale :: PrivateSale -> [TxHash]
-- getTxhsByPrivateSale ps = (^. invTx) <$> getPsInvestments ps

fstTriple :: (a, b, c) -> a
fstTriple (x, _, _) = x

sndTriple :: (a, b, c) -> b
sndTriple (_, y, _) = y

thrdTriple :: (a, b, c) -> c
thrdTriple (_, _, z) = z

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
  -- psInvestments ps :: Map TxHash Value
  --   mapM_ $ verifyTx $ getPsInvestments ps
  mapM_ $ verifyTx $ toList $ psInvestments ps
  where
    verifyTx ::
      forall (m :: Type -> Type).
      ( MonadIO m
      , MonadError TokenomiaError m
      , MonadReader Environment m
      ) =>
      [(TxHash, Value)]
      TxHash ->
      m ()
    verifyTx invs txh = do
      bfTxUtxos <- getTxUtxosByTxHash txh
      inv <- liftEither $ getValueByTxHash txh invs -- :: Value
      let bfUtxoOutputs = bfTxUtxos ^. outputs -- :: [UtxoOutput]
          treasAddrOutputs =
            filter (\output -> output ^. address == psAddress ps) bfUtxoOutputs -- :: [UtxoOutput]
          bfAmts = concat ((^. amount) <$> treasAddrOutputs) -- :: [Amount]
          bfValues = amountToAssetValue <$> bfAmts -- [(AssetClass, Integer)], equivalent to [Investment]
          totalAmount = sumRelevantValues inv bfValues -- :: Integer
          invValue = (flattenValue inv) -- :: [(CurrencySymbol, TokenName, Integer)]
        --   invAssetClass' = inv ^. invAssetClass -- need some fxn to extract AssetClass from Value
      unless (thrdTriple invValue == confirmedVals && invAssetClass' == inv ^. invAssetClass) $
        throwError . BlockFrostError . BlockfrostError $ "Values don't match"

--   getValueByTxHash ::
--   TxHash ->
--   [(TxHash, Value)]
--   Either TokenomiaError Value
-- getValueByTxHash txh invs =
--   maybeToRight
--     ( BlockFrostError . BlockfrostError $
--         "Investment list doesn't contain matching TxHash"
--     )
--     $ snd . find $ (\inv -> fst inv == txh) invs


sumRelevantValues :: Investment -> [(AssetClass, Integer)] -> Integer
sumRelevantValues inv = foldr (\(ac, amt) z -> if ac == inv ^. invAssetClass then amt + z else z) 0
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

--should search through a Map instead of [Investment]
--actually shouldn't return a map because i want a single (TxHash, Value)
getValueByTxHash ::
  TxHash ->
  [(TxHash, Value)]
  Either TokenomiaError Value
getValueByTxHash txh invs =
  maybeToRight
    ( BlockFrostError . BlockfrostError $
        "Investment list doesn't contain matching TxHash"
    )
    $ snd . find $ (\inv -> fst inv == txh) invs

-- getPsInvestments ::
--   PrivateSale ->
--   [Investment]
-- getPsInvestments ps = concat ((^. piInvestments) <$> investors)
--   where
--     investors :: [PrivateInvestor]
--     investors = ps ^. psInvestors

--TODO: just pass it the whole Map and return a new Map after filtering by txhash?
-- Map TxHash Value
-- toList Map == [(TxHash, Value)]

--TODO: currently returns a list :: [TxHash]
verifyTxHashList ::
  Map TxHash Value ->
  [TxHash] ->
  [TxHash]
verifyTxHashList invMap treasTxhs =
  filter (`elem` treasTxhs) (fst <$> Map.toList invMap)

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
