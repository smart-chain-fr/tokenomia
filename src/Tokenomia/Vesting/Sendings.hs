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

module Tokenomia.Vesting.Sendings (Sendings (Sendings), MonadRunBlockfrost (getAddressTransactions, getTxUtxos), checkMalformedAddr, jsonToSendings, verifySendings, verifySendings') where

import Blockfrost.Client (
  Address,
  AddressTransaction,
  Amount (AdaAmount, AssetAmount),
  TransactionUtxos,
  TxHash,
 )
import Blockfrost.Client qualified as Client
import Blockfrost.Lens (address, amount, outputs, txHash)
import Blockfrost.Types (unAddress)
import Control.Lens ((^.))
import Control.Monad (unless, when)
import Control.Monad.Except (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Identity (IdentityT (IdentityT))
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, eitherDecode)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as ByteString
import Data.Default (def)
import Data.Hex (unhex)
import Data.Kind (Type)
import Data.List (partition)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.String (fromString)
import Data.Text (unpack)
import GHC.Generics (Generic)
import Ledger.Value (AssetClass, CurrencySymbol, TokenName, Value, assetClass, assetClassValue, flattenValue, isZero)
import Money qualified
import Tokenomia.Common.Blockfrost (projectFromEnv'')
import Tokenomia.Common.Environment (Environment)
import Tokenomia.Common.Error (TokenomiaError (BlockFrostError, MalformedAddress, SendingsContainsZeroValue, SendingsJSONDecodingFailure, SendingsNoSuchTransactions, SendingsValueMismatch))
import Tokenomia.TokenDistribution.Parser.Address (deserialiseCardanoAddress)

data Sendings = Sendings
  { sendingsRecipientAddress :: Address
  , sendingsTxValues :: NEMap TxHash Value
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
  when (any isZero (sendingsTxValues sendings)) $
    throwError SendingsContainsZeroValue

  checkedAddr <- checkMalformedAddr $ sendingsRecipientAddress sendings

  treasAddrTxs <- getAddressTransactions checkedAddr
  let treasTxhs = (^. txHash) <$> treasAddrTxs
      flatSendingsTxValues = NonEmpty.toList . NEMap.toList . sendingsTxValues $ sendings
      (txhs, missingTxhs) = verifyTxHashList flatSendingsTxValues treasTxhs
  if length txhs == length flatSendingsTxValues
    then verifyTxs sendings
    else throwError . SendingsNoSuchTransactions $ missingTxhs

checkMalformedAddr ::
  forall (m :: Type -> Type).
  ( MonadError TokenomiaError m
  ) =>
  Address ->
  m Address
checkMalformedAddr addr =
  addr <$ (liftEither . first (const MalformedAddress) $ deserialiseCardanoAddress (unAddress addr))

jsonToSendings ::
  forall (m :: Type -> Type).
  ( MonadError TokenomiaError m
  ) =>
  ByteString.ByteString ->
  m Sendings
jsonToSendings jsonByteString = do
  let eitherErrSendings = eitherDecode jsonByteString
  liftEither $ first SendingsJSONDecodingFailure eitherErrSendings

verifyTxHashList ::
  [(TxHash, Value)] ->
  [TxHash] ->
  ([TxHash], [TxHash])
verifyTxHashList flatSendingsTxValues treasTxhs =
  partition (`elem` treasTxhs) (fst <$> flatSendingsTxValues)

verifyTxs ::
  forall (m :: Type -> Type).
  ( MonadRunBlockfrost m
  , MonadError TokenomiaError m
  ) =>
  Sendings ->
  m ()
verifyTxs sendings =
  mapM_ verifyTx $ NEMap.toList $ sendingsTxValues sendings
  where
    verifyTx ::
      forall (m :: Type -> Type).
      ( MonadRunBlockfrost m
      , MonadError TokenomiaError m
      ) =>
      (TxHash, Value) ->
      m ()
    verifyTx (txh, txValue) = do
      bfTxUtxos <- getTxUtxos txh
      let treasAddrOutputs =
            filter
              ((== sendingsRecipientAddress sendings) . (^. address))
              (bfTxUtxos ^. outputs)
          bfAmts = concat ((^. amount) <$> treasAddrOutputs)
          bfVals = amountToAssetValue <$> bfAmts

          flatVal = fmap (\(c, t, i) -> (assetClass c t, i)) . flattenValue $ txValue
          -- Keep only relevant assets
          bfValsFiltered = filter ((`elem` map fst flatVal) . fst) bfVals

          -- Sum sendings and blockfrost totals
          sendingsTotal = foldMap (uncurry assetClassValue) flatVal
          bfValsTotal = foldMap (uncurry assetClassValue) bfValsFiltered
      unless (sendingsTotal == bfValsTotal) $
        throwError . SendingsValueMismatch $ (sendingsTotal, bfValsTotal)

flatValToAssetClass :: (CurrencySymbol, TokenName, Integer) -> AssetClass
flatValToAssetClass (cs, tn, _) = assetClass cs tn

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
