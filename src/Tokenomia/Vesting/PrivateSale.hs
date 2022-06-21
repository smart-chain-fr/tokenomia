{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tokenomia.Vesting.PrivateSale (verifyPrivateSale) where

import Ledger hiding (mint, singleton)
import Ledger.Value as Value
import Prelude hiding (readFile)

import PlutusTx.Prelude (fromBuiltin)

import Control.Monad hiding (fmap)
import Control.Monad.Reader hiding (ask)
import GHC.Generics (Generic)

import Control.Lens
import Control.Monad.Except
import Data.Aeson (FromJSON, decode, eitherDecodeFileStrict)
import Data.ByteString.Internal (unpackChars)
import Data.ByteString.Lazy (readFile)
import Data.Foldable (find, foldr1)
import Data.Function (on)
import Data.List (sort)
import Data.Text (Text, pack)
import Data.Time (UTCTime (UTCTime))
import Money
import System.IO (FilePath, getLine, putStrLn)

import qualified Blockfrost.Client as B
import qualified Blockfrost.Lens as B

import Tokenomia.Common.Blockfrost (projectFromEnv'')
import Tokenomia.Common.Environment
import Tokenomia.Common.Error

instance FromJSON PrivateSale
instance FromJSON PrivateInvestor
instance FromJSON Investment
instance FromJSON Tranche

data PrivateSale = PrivateSale
    { _psAddress :: B.Address -- Treasury address
    , _psStart :: POSIXTime
    , _psTranches :: [Tranche]
    , _psAssetClass :: AssetClass -- Vesting token
    , _psInvestors :: [PrivateInvestor]
    }
    deriving (Generic, Show)

data Tranche = Tranche
    { _tranchePercentage :: Integer
    , _trancheDuration :: Integer
    }
    deriving (Generic, Show)

data PrivateInvestor = PrivateInvestor
    { _piAddress :: B.Address
    , _piAllocation :: Integer -- Amount of vesting tokens to lock (note, this is likely NOT equal to the total amounts of investments, as different tokens)
    , _piInvestments :: [Investment]
    }
    deriving (Generic, Show)

data Investment = Investment
    { _invTx :: B.TxHash -- Tx that sends below asset class to payment address
    , _invAssetClass :: AssetClass
    , _invAmount :: Integer -- Amount of above asset class expected to be sent to payment address
    }
    deriving (Generic, Show)

makeLenses ''PrivateSale
makeLenses ''Tranche
makeLenses ''PrivateInvestor
makeLenses ''Investment

verifyPrivateSale ::
    ( MonadIO m
    , MonadError TokenomiaError m
    , MonadReader Environment m
    ) =>
    m ()
verifyPrivateSale = do
    liftIO . putStrLn $ "Please enter a filepath with JSON data"
    jsonFilePath <- liftIO getLine
    verifyPrivateSale' jsonFilePath
    return ()

verifyPrivateSale' ::
    ( MonadIO m
    , MonadError TokenomiaError m
    , MonadReader Environment m
    ) =>
    FilePath ->
    m ()
verifyPrivateSale' jsonFilePath = do
    ps <- jsonToPrivateSale jsonFilePath
    treasAddrTxs <- getTreasAddrTxs ps -- :: [B.AddressTransaction] <- m [B.AddressTransaction]
    let invTxhs = getTxhsByPrivateSale ps
        txhs = verifyTxHashList invTxhs treasAddrTxs
     in if length txhs == length invTxhs
            then do
                verifyTxs (getPsInvestments ps) txhs
            else throwError . BlockFrostError . B.BlockfrostError $ "Unequal lengths, means missing TXs"
    return ()

jsonToPrivateSale ::
    ( MonadIO m
    , MonadError TokenomiaError m
    , MonadReader Environment m
    ) =>
    FilePath ->
    m PrivateSale
-- eitherDecodeFileStrict :: FromJSON a => FilePath -> IO (Either String a)
jsonToPrivateSale jsonFilePath = do
    fileContents <- liftIO . readFile $ jsonFilePath
    case (decode fileContents :: Maybe PrivateSale) of
        Nothing -> throwError . BlockFrostError . B.BlockfrostError $ "Unable to parse JSON"
        Just ps -> return ps

getTxhsByPrivateSale :: PrivateSale -> [B.TxHash]
getTxhsByPrivateSale ps = invTxhs
  where
    invTxhs :: [B.TxHash]
    invTxhs = (^. invTx) <$> invs

    invs :: [Investment]
    invs = getPsInvestments ps

verifyTxs ::
    ( MonadIO m
    , MonadError TokenomiaError m
    , MonadReader Environment m
    ) =>
    [Investment] ->
    [B.TxHash] ->
    m ()
verifyTxs invs = mapM_ (verifyTx invs)
  where
    verifyTx ::
        ( MonadIO m
        , MonadError TokenomiaError m
        , MonadReader Environment m
        ) =>
        [Investment] ->
        B.TxHash ->
        m ()
    verifyTx invs txh = do
        bfTx <- getTxByTxHash txh
        inv <- liftEither $ getInvByTxHash txh invs
        let invTokenName = getTokenName $ inv ^. invAssetClass -- :: Text
            invAmount' = inv ^. invAmount -- :: Integer
            bfAmts = bfTx ^. B.outputAmount -- :: [B.Amount]
            amt = getAmountByTokenName invTokenName bfAmts -- :: B.Amount
        if getDiscreteAmount amt == invAmount'
            then return ()
            else throwError . BlockFrostError . B.BlockfrostError $ "Amounts don't match"

getTreasAddrTxs ::
    ( MonadIO m
    , MonadError TokenomiaError m
    , MonadReader Environment m
    ) =>
    PrivateSale ->
    m [B.AddressTransaction]
getTreasAddrTxs ps =
    do
        prj <- projectFromEnv''
        liftIO $
            B.runBlockfrost prj $ do
                B.getAddressTransactions (ps ^. psAddress)
        >>= \case
            Left e -> throwError $ BlockFrostError e
            Right res -> return res

getTxByTxHash ::
    ( MonadIO m
    , MonadError TokenomiaError m
    , MonadReader Environment m
    ) =>
    B.TxHash ->
    m B.Transaction
getTxByTxHash txh =
    do
        prj <- projectFromEnv''
        liftIO $
            B.runBlockfrost prj $ do
                B.getTx txh
        >>= \case
            Left e -> throwError $ BlockFrostError e
            Right res -> return res

getTokenName :: AssetClass -> Text
getTokenName = pack . unpackChars . fromBuiltin . unTokenName . snd . unAssetClass

getInvByTxHash :: B.TxHash -> [Investment] -> Either TokenomiaError Investment
getInvByTxHash txh (inv : invs) =
    case find (\inv -> (inv ^. invTx) == txh) invs of
        Nothing -> throwError . BlockFrostError . B.BlockfrostError $ "Investment list is empty"
        Just res -> Right res

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

-- getAmountByTokenName :: Text -> [B.Amount] -> Maybe B.Amount
--TODO: don't use foldr1
getAmountByTokenName :: Text -> [B.Amount] -> B.Amount
getAmountByTokenName tn = foldr1 (\amt z -> if getDiscreteCurrency amt == tn then amt else z)

-- getAmountByTokenName tn amts =
--     case find (\amt -> getDiscreteCurrency amt == tn) amts of
--         Nothing -> throwError . BlockFrostError . B.BlockfrostError $ "No Amount matching this token name"
--         Just amt -> amt

getDiscreteCurrency :: B.Amount -> Text
getDiscreteCurrency (B.AdaAmount ll) = "ADA"
getDiscreteCurrency (B.AssetAmount sd) = someDiscreteCurrency sd

getDiscreteAmount :: B.Amount -> Integer
getDiscreteAmount (B.AdaAmount ll) = someDiscreteAmount . toSomeDiscrete $ ll
getDiscreteAmount (B.AssetAmount sd) = someDiscreteAmount sd

-- getInvByTxHash _ [] = throwError . BlockFrostError . B.BlockfrostError $ "Investment list is empty"
-- getInvByTxHash txh (inv : invs) = Right . head . filter (\inv -> (inv ^. invTx) == txh) $ invs

-- getInvByTxHash :: B.TxHash -> [Investment] -> Maybe Investment
-- getInvByTxHash _ [] = Nothing
-- getInvByTxHash txh invs = Just . head . filter (\inv -> (inv ^. invTx) == txh) $ invs
--TODO: FIX!!
-- TokenomiaError m => Maybe a -> TokenomiaError -> m a
-- getInvByTxHash :: B.TxHash -> [Investment] -> Maybe Investment
-- getInvByTxHash :: B.TxHash -> [Investment] -> Investment

-- TODO: maybe use Data.Foldable.find to get the left-most thing matching a predicate