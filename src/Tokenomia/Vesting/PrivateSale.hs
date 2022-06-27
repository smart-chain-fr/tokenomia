{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

import Debug.Trace

import Data.Hex (unhex)
import Data.String (fromString)

import Control.Lens
import Control.Monad.Except
import Data.Aeson (FromJSON, decode, eitherDecodeFileStrict)
import Data.Bifunctor (first)
import Data.ByteString.Internal (unpackChars)
import Data.ByteString.Lazy (readFile)
import Data.Foldable (find)
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime (UTCTime))
import Money
import System.IO (FilePath, getLine, putStrLn)

import qualified Blockfrost.Client as B
import qualified Blockfrost.Lens as B

import Tokenomia.Common.Blockfrost (projectFromEnv'')
import Tokenomia.Common.Environment
import Tokenomia.Common.Error

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
    if length txhs == length invTxhs
        then do
            verifyTxs ps txhs
        else throwError . BlockFrostError . B.BlockfrostError $ "Missing Transactions"

jsonToPrivateSale ::
    ( MonadIO m
    , MonadError TokenomiaError m
    , MonadReader Environment m
    ) =>
    FilePath ->
    m PrivateSale
jsonToPrivateSale jsonFilePath = do
    eitherErrPs <- liftIO . eitherDecodeFileStrict $ jsonFilePath
    liftEither (first (BlockFrostError . B.BlockfrostError . pack) eitherErrPs)


--     jsonToPrivateSale ::
--     ( MonadIO m
--     , MonadError TokenomiaError m
--     , MonadReader Environment m
--     ) =>
--     FilePath ->
--     m PrivateSale
-- jsonToPrivateSale jsonFilePath = do
--     eitherErrPs <- liftIO . eitherDecodeFileStrict $ jsonFilePath
--     liftEither (first (BlockFrostError . B.BlockfrostError . pack) eitherErrPs)


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
                -- eitherErrAddrTxs <- 
        >>= \case
            Left e -> throwError $ BlockFrostError e
            Right res -> return res

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
    PrivateSale ->
    [B.TxHash] ->
    m ()
verifyTxs ps =
    let invs = getPsInvestments ps
     in mapM_ (verifyTx invs)
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
        bfTxUtxos <- getTxUtxosByTxHash txh -- :: B.TransactionUtxos
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

getTxUtxosByTxHash ::
    ( MonadIO m
    , MonadError TokenomiaError m
    , MonadReader Environment m
    ) =>
    B.TxHash ->
    m B.TransactionUtxos
getTxUtxosByTxHash txh =
    do
        prj <- projectFromEnv''
        liftIO $
            B.runBlockfrost prj $ do
                B.getTxUtxos txh
        >>= \case
            Left e -> throwError $ BlockFrostError e
            Right res -> return res

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

-- case find (\inv -> (inv ^. invTx) == txh) invs of
--     Nothing -> Left . BlockFrostError . B.BlockfrostError $ "Investment list is empty"
--     Just res -> Right res

-- if getDiscreteAmount amt == invAmount'
--     then return ()
--     else throwError . BlockFrostError . B.BlockfrostError $ "Amounts don't match"

-- amt = getAmountByTokenName invTokenName bfAmts -- :: B.Amount

--  decode :: FromJSON a => ByteString -> Maybe a
-- liftEither . Data.Bifunctor.first (BlockFrostError . B.BlockfrostError)
-- fileContents <- liftIO . readFile $ jsonFilePath

-- case (decode fileContents :: Maybe PrivateSale) of
--     Nothing -> throwError . BlockFrostError . B.BlockfrostError $ "Unable to parse JSON"
--     Just ps -> return ps

-- getAmountByTokenName tn amts =
--     case find (\amt -> getDiscreteCurrency amt == tn) amts of
--         Nothing -> throwError . BlockFrostError . B.BlockfrostError $ "No Amount matching this token name"
--         Just amt -> amt

-- liftEither (bimap (BlockFrostError . B.BlockfrostError) pack eitherErrPs)

-- eitherDecodeFileStrict jsonFilePath >>= (liftEither . first (throwError . BlockFrostError . B.BlockfrostError))

-- eitherDecodeFileStrict :: FromJSON a => FilePath -> IO (Either String a)
-- first :: (a -> b) -> p a c -> p b c
-- liftEither :: MonadError e m => Either e a -> m a
-- liftEither (first (throwError . BlockFrostError . B.BlockfrostError) eitherVal)

--TODO use -XTypeApplications ?

-- getInvByTxHash txh [] = Left . BlockFrostError . B.BlockfrostError $ "Investment list is empty"
-- getInvByTxHash txh (inv : invs) =
--     maybe
--         ( Left . BlockFrostError . B.BlockfrostError $
--             "Investment list is empty"
--         )
--         Right
--         (find (\inv -> (inv ^. invTx) == txh) invs)

--     Left . BlockFrostError . B.BlockfrostError $
--         "No Amount matching this token name"
-- getAmountByTokenName tn (amt : amts) =
--     maybe
--         ( Left . BlockFrostError . B.BlockfrostError $
--             "No Amount matching this token name"
--         )
--         Right
--         (find (\amt -> getDiscreteCurrency amt == tn) amts)

-- getTxByTxHash ::
--     ( MonadIO m
--     , MonadError TokenomiaError m
--     , MonadReader Environment m
--     ) =>
--     B.TxHash ->
--     m B.Transaction
-- getTxByTxHash txh =
--     do
--         prj <- projectFromEnv''
--         liftIO $
--             B.runBlockfrost prj $ do
--                 B.getTx txh
--         >>= \case
--             Left e -> throwError $ BlockFrostError e
--             Right res -> return res

-- bfAmts = bfTx ^. B.outputAmount -- :: [B.Amount]

-- let invTokenName = getTokenName $ inv ^. invAssetClass -- :: Text

--TODO search by assetclass rather than tokenname
--    compare currsym with getDiscreteCurrency and tokenname with getName

-- getAmountByTokenName :: Text -> [B.Amount] -> Either TokenomiaError B.Amount
-- getAmountByTokenName tn amts =
--     maybe
--         ( Left . BlockFrostError . B.BlockfrostError $
--             "No Amount matching this token name"
--         )
--         Right
--         (find (\amt -> getDiscreteCurrency amt == tn) amts)

-- getDiscreteCurrency :: B.Amount -> Text
-- getDiscreteCurrency (B.AdaAmount ll) = "ADA"
-- getDiscreteCurrency (B.AssetAmount sd) = someDiscreteCurrency sd

-- getDiscreteAmount :: B.Amount -> Integer
-- getDiscreteAmount (B.AdaAmount ll) = someDiscreteAmount . toSomeDiscrete $ ll
-- getDiscreteAmount (B.AssetAmount sd) = someDiscreteAmount sd

--TODO: now filter the UtxoOutputs for those that send to the treasury addr, and tokenname / currsym match
-- lens to get utxoOutputAddress: address, -- (^. B.address) <$> bfUtxoOutputs :: [B.Address]
-- lens to get tokenname / currsym (this comes from his fxn, so I just need Amount): (^. amount) <$> bfUtxoOutputs :: [[B.Amount]]
-- amountToAssetValue <$> treasAddrUtxos
--TODO: flatten with concat?
--  in do
-- amt <- liftEither $ getAmountByTokenName invTokenName values

-- unless (traceShowId (getDiscreteAmount amt) == traceShowId invAmount') $
--     throwError . BlockFrostError . B.BlockfrostError $ "Amounts don't match"

--TODO: the acc for the fold can just be the amount, and at the last step check that the amounts match
--    TRACE: it's failing at the check with fst val == ps ^. psAssetClass

-- snd val + z
-- getAmountByTokenName :: Text -> [B.Amount] -> Either TokenomiaError B.Amount
-- getAmountByTokenName tn amts =
--     maybe
--         ( Left . BlockFrostError . B.BlockfrostError $
--             "No Amount matching this token name"
--         )
--         Right
--         (find (\amt -> getDiscreteCurrency amt == tn) amts)

-- getTokenName :: Value.AssetClass -> Text
-- getTokenName = pack . unpackChars . fromBuiltin . unTokenName . snd . unAssetClass
