{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tokenomia.Vesting.PrivateSale (verifyPrivateSale) where

import Ledger hiding (mint, singleton)
import Ledger.Value as Value
import Prelude hiding (readFile)

import Control.Monad hiding (fmap)
import Control.Monad.Reader
import GHC.Generics (Generic)

import Debug.Trace

import Data.Hex (unhex)
import Data.String (fromString)

import Control.Lens
import Control.Monad.Except
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Bifunctor (first)
import Data.Foldable (find)
import Data.Text (pack, unpack)
import Money
-- import System.IO (FilePath, getLine, putStrLn)

import qualified Blockfrost.Client as B
-- import qualified Blockfrost.Lens   as B

import Tokenomia.Common.Blockfrost (projectFromEnv'')
import Tokenomia.Common.Environment
import Tokenomia.Common.Error
-- import Control.Monad.Identity (IdentityT)
import Control.Monad.Trans.Identity

data PrivateSale = PrivateSale
    { _psAddress :: B.Address -- Treasury address
    , _psStart :: POSIXTime
    , _psTranches :: [Tranche]
    , _psAssetClass :: Value.AssetClass -- Vesting token
    , _psInvestors :: [PrivateInvestor]
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
    { _piAddress :: B.Address
    , _piAllocation :: Integer -- Amount of vesting tokens to lock (note, this is likely NOT equal to the total amounts of investments, as different tokens)
    , _piInvestments :: [Investment]
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

data Investment = Investment
    { _invTx :: B.TxHash -- Tx that sends below asset class to payment address
    , _invAssetClass :: Value.AssetClass
    , _invAmount :: Integer -- Amount of above asset class expected to be sent to payment address
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

makeLenses ''PrivateSale
makeLenses ''Tranche
makeLenses ''PrivateInvestor
makeLenses ''Investment


class Monad m => MonadRunBlockfrost m where
  getAddressTransactions :: B.Address -> m [B.AddressTransaction]
  getTxUtxos :: B.TxHash -> m B.TransactionUtxos

newtype RealBlockfrost (m :: * -> *) (a :: *) =
  RealBlockfrost {runRealBlockfrost :: m a}
  deriving (Functor, Applicative, Monad) via IdentityT m

newtype FakeBlockfrost (m :: * -> *) (a :: *) = 
  FakeBlockfrost { runFakeBlockfrost :: m a } 
  deriving (Functor, Applicative, Monad) via IdentityT m

instance (MonadReader Environment m) => MonadReader Environment (RealBlockfrost m) where
  ask = RealBlockfrost ask
  -- reader = RealBlockfrost . reader
  local f = RealBlockfrost . local f . runRealBlockfrost

instance (MonadError TokenomiaError m) => MonadError TokenomiaError (RealBlockfrost m) where
  throwError = RealBlockfrost . throwError
  catchError (RealBlockfrost m) f = RealBlockfrost $ catchError m (runRealBlockfrost . f)


instance (MonadIO m, MonadReader Environment m, MonadError TokenomiaError m) => MonadRunBlockfrost (RealBlockfrost m) where
  getAddressTransactions ad = RealBlockfrost $ do
     prj <- projectFromEnv''
     eitherErrAddrTxs <- liftIO $ B.runBlockfrost prj (B.getAddressTransactions ad)
     liftEither $ first BlockFrostError eitherErrAddrTxs
  getTxUtxos txh = RealBlockfrost $ do
      prj <- projectFromEnv''
      eitherErrTxUtxos <- liftIO $ B.runBlockfrost prj (B.getTxUtxos txh)
      liftEither $ first BlockFrostError eitherErrTxUtxos

verifyPrivateSale ::
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
    ( MonadRunBlockfrost m
    , MonadError TokenomiaError m
    ) =>
    PrivateSale ->
    m ()
verifyPrivateSale' ps = do
    treasAddrTxs <- getAddressTransactions (ps ^. psAddress) -- :: [B.AddressTransaction] <- m [B.AddressTransaction]
    let invTxhs = getTxhsByPrivateSale ps
        txhs = verifyTxHashList invTxhs treasAddrTxs
    if length txhs == length invTxhs
        then do
            verifyTxs ps txhs
        else throwError . BlockFrostError . B.BlockfrostError $ "Missing Transactions"

jsonToPrivateSale ::
    ( MonadIO m
    , MonadError TokenomiaError m
    ) =>
    FilePath ->
    m PrivateSale
jsonToPrivateSale jsonFilePath = do
    eitherErrPs <- liftIO . eitherDecodeFileStrict $ jsonFilePath
    liftEither (first (BlockFrostError . B.BlockfrostError . pack) eitherErrPs)

getTxhsByPrivateSale :: PrivateSale -> [B.TxHash]
getTxhsByPrivateSale ps = invTxhs
  where
    invTxhs :: [B.TxHash]
    invTxhs = (^. invTx) <$> invs

    invs :: [Investment]
    invs = getPsInvestments ps

verifyTxs ::
    ( MonadRunBlockfrost m
    , MonadError TokenomiaError m
    ) =>
    PrivateSale ->
    [B.TxHash] ->
    m ()
verifyTxs ps =
    let invs = getPsInvestments ps
     in mapM_ (verifyTx invs)
  where
    verifyTx ::
        ( MonadRunBlockfrost m
        , MonadError TokenomiaError m
        ) =>
        [Investment] ->
        B.TxHash ->
        m ()
    verifyTx invs txh = do
        bfTxUtxos <- getTxUtxos txh -- :: B.TransactionUtxos
        inv <- liftEither $ getInvByTxHash txh invs -- :: Investment
        let bfUtxoOutputs = bfTxUtxos ^. B.outputs -- :: [B.UtxoOutput]
            treasAddrOutputs = filter (\output -> (output ^. B.address) == (ps ^. psAddress)) bfUtxoOutputs -- :: [B.UtxoOutput]
            bfAmts = concat ((^. B.amount) <$> treasAddrOutputs) -- :: [B.Amount]
            bfValues = amountToAssetValue <$> bfAmts -- :: [(Value.AssetClass, Integer)]
            confirmedVals = confirmValues ps (traceShowId bfValues) -- :: Integer
            invAmount' = inv ^. invAmount -- :: Integer
            invAssetClass' = inv ^. invAssetClass -- :: Value.AssetClass
         in unless (invAssetClass' == ps ^. psAssetClass && confirmedVals == invAmount') $
                throwError . BlockFrostError . B.BlockfrostError $ "Values don't match"

confirmValues :: PrivateSale -> [(Value.AssetClass, Integer)] -> Integer
confirmValues ps = foldr (\val z -> if fst val == ps ^. psAssetClass then snd val + z else z) 0


getInvByTxHash :: B.TxHash -> [Investment] -> Either TokenomiaError Investment
getInvByTxHash txh invs =
    maybe
        ( Left . BlockFrostError . B.BlockfrostError $
            "Investment list doesn't contain matching TxHash"
        )
        Right
        (find (\inv -> (inv ^. invTx) == txh) invs)

getPsInvestments ::
    PrivateSale ->
    [Investment]
getPsInvestments ps = investments
  where
    investments :: [Investment]
    investments = concat ((^. piInvestments) <$> investors)

    investors :: [PrivateInvestor]
    investors = ps ^. psInvestors

verifyTxHashList :: [B.TxHash] -> [B.AddressTransaction] -> [B.TxHash]
verifyTxHashList txhs addrTxs =
    filter (`elem` txhs) ((^. B.txHash) <$> addrTxs)

amountToAssetValue :: B.Amount -> (Value.AssetClass, Integer)
amountToAssetValue (B.AdaAmount ll) = (Value.assetClass "" "", someDiscreteAmount $ toSomeDiscrete ll)
amountToAssetValue (B.AssetAmount sd) = (Value.assetClass (fromString cs) (fromString name), someDiscreteAmount sd)
  where
    encodedName = unpack $ someDiscreteCurrency sd
    cs = take 56 encodedName
    name = either error id $ unhex $ drop 56 encodedName
