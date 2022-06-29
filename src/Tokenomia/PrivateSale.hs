{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
<<<<<<< HEAD:src/Tokenomia/Vesting/PrivateSale.hs
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tokenomia.Vesting.PrivateSale (verifyPrivateSale) where

import Ledger hiding (mint, singleton)
import Ledger.Value as Value
import Prelude hiding (readFile)

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
import Data.ByteString.Lazy (readFile)
import Data.Foldable (find)
import Data.Text (Text, pack, unpack)
import Money
import System.IO (FilePath, getLine, putStrLn)

import qualified Blockfrost.Client as B
import qualified Blockfrost.Lens   as B

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
=======
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module PrivateSale where

-- TODO: add necessary imports, clear unnecessary imports 
import           Prelude                (IO, Semigroup (..), Show (..), String)
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Value           as Value
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Ada
import           Control.Lens 
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, decode)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           System.IO              (putStrLn, getLine)
import qualified Data.ByteString.Lazy as BS
import           Data.Time              (UTCTime(UTCTime))
import           Data.Function          (on)
import           Data.List              (sort)
import           Money.SomeDiscrete     (someDiscreteAmount, someDiscreteCurrency)
import qualified Blockfrost.Client    as B

type Percentage = Integer 
type Duration   = Integer 

instance FromJSON PrivateSale
instance FromJSON PrivateInvestor 
instance FromJSON Investment 
instance FromJSON Tranche 

{-
Everything in this file has yet to be integrated, once the logic is complete I can 
integrate it into the CLI quite easily, exporting relevant fxns. 
For now it's disorganized and incomplete. 
*************************************************************************************
STRUCTURE:
----------
I'm going to build based on the assumption that we have access to TxHash in the JSON
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
If we have access to TxId/TxHash, then simply pull the Tx's from treasury address using

  getAddressTransactions :: MonadBlockfrost m => Address -> m [AddressTransaction]

On each AddressTransaction, we have a TxHash.

We then pull the TxHashes from Investment, filtering [AddressTransaction] by these TxHashes,
comparing the Amount and AssetClass

We'll need to access fields of Investment using optics b/c nested records
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

But if we don't have access to TxId/TxHash in Investor, then we have 2 other options:

We can search by PrivateInvestor address. Using the getAddressTransactions again,
we get [AddressTransaction], from which we can extract the [TxHash], which we can use
to pull up the full Transaction using 

  getTx :: MonadBlockfrost m => TxHash -> m Transaction

We have a lot more fields to look at in the Transaction type, but still we can't just see
addresses / UTXOs

So then maybe use

  getTxUtxos :: MonadBlockfrost m => TxHash -> m TransactionUtxos

And the type TransactionUtxos has a field _transactionUtxosOutputs :: [UtxoOutput]

And UtxoOutput has _utxoOutputAddress :: Address and _utxoOutputAmount :: [Amount]

So we can filter _utxoOutputAddress by the treasury address and make sure the Amount is correct,
but we're not confirming the AssetClass here. To do that, we should pull the AssetClass from
Investor and use 

  getAssetTransactions :: MonadBlockfrost m => AssetId -> m [AssetTransaction]

from which we can extract TxHash just like with AddressTransaction, and filter those TxHash
by the ones we got earlier.

*************************************************************************************
-}


data PrivateSale
  = PrivateSale
    { _psAddress         :: Address    -- Treasury address
    , _psStart           :: POSIXTime
    , _psTranches        :: [Tranche]
    , _psAssetClass      :: AssetClass -- Vesting token
    , _psInvestors       :: [PrivateInvestor]
    } deriving (Generic, Show)

data Tranche 
  = Tranche
    { _tranchePercentage :: Percentage
    , _trancheDuration   :: Duration 
    } deriving (Generic, Show)

data PrivateInvestor
  = PrivateInvestor
    { _piAddress         :: Address
    , _piAllocation      :: Integer -- Amount of vesting tokens to lock (note, this is likely NOT equal to the total amounts of investments, as different tokens)
    , _piInvestments     :: [Investment]
    } deriving (Generic, Show)

data Investment
  = Investment
    { _invTx             :: TxId    -- Tx that sends below asset class to payment address
    , _invAssetClass     :: AssetClass
    , _invAmount         :: Integer -- Amount of above asset class expected to be sent to payment address
    } deriving (Generic, Show)
>>>>>>> parent of 6e584dc (Added logic for comparing AssetClass & Amount):src/Tokenomia/PrivateSale.hs

makeLenses ''PrivateSale
makeLenses ''Tranche
makeLenses ''PrivateInvestor
makeLenses ''Investment

<<<<<<< HEAD:src/Tokenomia/Vesting/PrivateSale.hs
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

getTreasAddrTxs ::
    ( MonadIO m
    , MonadError TokenomiaError m
    , MonadReader Environment m
    ) =>
    PrivateSale ->
    m [B.AddressTransaction]
getTreasAddrTxs ps = do
    prj <- projectFromEnv''
    eitherErrAddrTxs <- liftIO $ B.runBlockfrost prj (B.getAddressTransactions (ps ^. psAddress))
    liftEither $ first BlockFrostError eitherErrAddrTxs

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
getTxUtxosByTxHash txh = do
    prj <- projectFromEnv''
    eitherErrTxUtxos <- liftIO $ B.runBlockfrost prj (B.getTxUtxos txh)
    liftEither $ first BlockFrostError eitherErrTxUtxos

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
=======
{-
look up txId and verify that amount and assetClass are the same as what we pulled from address
  address privateSale ... This is the treasury address 
  using getAddressTransactions I can pull up all the Tx's for that address:
  getAddressTransactions :: MonadBlockfrost m => Address -> m [AddressTransaction] -- Each AddrTx will have a TxHash
  then AddressTransaction has:

  _addressTransactionTxHash :: TxHash

  then using TxHash I can pull up Transaction with a ton of fields

-----------------------------------------------
  
  PrivateSale -> [PrivateInvestor] -> [[Investment]] -> TxId ... we use this TxId to lookup assetclass / amount and compare to 
    treasury address info... but how?
    the following are all fxn's that take TxHash as a param (in blockfrost-client):
    getTx :: MonadBlockfrost m => TxHash -> m Transaction
    getTxUtxos :: MonadBlockfrost m => TxHash -> m TransactionUtxos

  If I have TxHash, then I could maybe use getTx:
  getTx :: MonadBlockfrost m => TxHash -> m Transaction

  Then Transaction has the following fields of interest:
  _transactionOutputAmount :: [Amount] -- where data Amount = AdaAmount Lovelaces | AssetAmount SomeDiscrete 
    SomeDiscrete appears to be for non-Ada assets, but it appears to only have a field for the token name, not the full AssetClass

note: the pair below for AssetAmount is pseudocode
  so it's: TxHash -> Transaction -> [Amount] -> [AssetAmount (amount, tokenName)] -> 

-----
***
If I don't have access to the TxId/TxHash for the Investment type, I can search by AssetClass by doing:

getAssetTransactions :: MonadBlockfrost m => AssetId -> m [AssetTransaction]

where AssetId is the currency symbol / tokenName concatenated 

-}

getPsInvestments :: PrivateSale -> [Investment]
getPsInvestments ps = investments 

>>>>>>> parent of 6e584dc (Added logic for comparing AssetClass & Amount):src/Tokenomia/PrivateSale.hs
  where
    investments :: [Investment]
    investments = concat ((^. piInvestments) <$> investors)

    investors :: [PrivateInvestor]
    investors = ps ^. psInvestors

verifyTxHashList :: [B.TxHash] -> [B.AddressTransaction] -> [B.TxHash]
verifyTxHashList txhs addrTxs =
    filter (`elem` txhs) ((^. B.txHash) <$> addrTxs)

<<<<<<< HEAD:src/Tokenomia/Vesting/PrivateSale.hs
amountToAssetValue :: B.Amount -> (Value.AssetClass, Integer)
amountToAssetValue (B.AdaAmount ll) = (Value.assetClass "" "", someDiscreteAmount $ toSomeDiscrete ll)
amountToAssetValue (B.AssetAmount sd) = (Value.assetClass (fromString cs) (fromString name), someDiscreteAmount sd)
  where
    encodedName = unpack $ someDiscreteCurrency sd
    cs = take 56 encodedName
    name = either error id $ unhex $ drop 56 encodedName
=======
    assetClasses :: [AssetClass]
    assetClasses = (^. invAssetClass) <$> getPsInvestments ps 

    amounts :: [Integer]
    amounts = (^. invAmount) <$> getPsInvestments ps

    sdTokenName :: Text
    sdTokenName = someDiscreteCurrency sd

    sdAmount :: Integer
    sdAmount = someDiscreteAmount sd

-- someDiscreteCurrency :: SomeDiscrete -> Text
-- someDiscreteAmount :: SomeDiscrete -> Integer

getTokenName :: AssetClass -> TokenName
getTokenName = snd . unAssetClass

-- TODO: abstract as much as possible out of this mess
main :: IO ()
main = do 
  putStrLn "Please enter the filepath to a private sale .json file"
  jsonFile <- getLine
  contents <- BS.readFile jsonFile
  case (decode contents :: Maybe PrivateSale) of
    Nothing -> putStrLn "Unable to parse"
    Just ps -> let treasuryTransactions = getAddressTransactions . psAddress $ ps in do -- V :: m [AddressTransaction]
      prj <- B.projectFromEnv''
      treasuryTransactions' <- liftIO (B.runBlockfrost prj treasuryTransactions) -- TODO: check types (is this [AddressTransaction] ?)
                                  >>= (\case 
                                          Left e    -> throwError $ BlockFrostError e
                                          Right res -> return res)
        let investmentTxHashes = (^. invTx) <$> getPsInvestments ps in -- [TxHash]
          let txhs = filter (`elem` investmentTxHashes) (_addressTransactionTxHash <$> treasuryTransactions') in -- :: [TxHash]

          -- TODO: abstract into its own fxn
            case (compare `on` length) txhs investmentTxHashes of -- TODO: is comparing the length enough to make sure
                                                                                  -- all the TXs are there? seems likely

-- TODO: probably don't need to use `on`, just compare length instead. no need to have a GT line when it won't execute anyway
-- TODO: decide if the fold that returns a list of transactions that didn't pass our comparison tests
-- TODO: compare return type of _transactionOutputAmount, i.e. [Amount] to Tx outputs
--   i.e. what we get from blockfrost for a given TxHash is [Amount]. But in the PrivateSale type, each Investment
--   has only 1 Amount (Integer) for each TxHash. Then we check if Integer <= sum $ [Amount] ... I think?
--   Or because Amount == AssetClass, maybe search [Amount] by TokenName, and for the Amount with matching
--   TokenName, make sure the Amount is the same
--   So... get TokenName first, then search through [[Amount]]
--   So I need to make use of these 2 fxn's used earlier:

              LT -> putStrLn "failed, missing TXs"
              GT -> putStrLn "this line will never execute"
              EQ -> let investments = getPsInvestments ps in 
                foldl (\z txh -> 
                  let inv = filterTransactions txh investments in
                    let assetClass = inv ^. invAssetClass in
                      let tokenName = getTokenName assetClass in
                        head . filter (== tokenName) (_transactionOutputAmount txh)
                  -- now we have the Investment that corresponds to this TxHash, so get the AssetClass and Integer from Investment

                  ) z txhs

tokenNames :: [TokenName]
tokenNames = getTokenName <$> assetClasses

assetClasses :: [AssetClass]
assetClasses = (^. invAssetClass) <$> getPsInvestments ps 

filterTransactions :: TxHash -> [Investment] -> Investment
filterTransactions txh = head . filter (== txh)            
                
              -- so we have filteredTransactions (treas) [TxHash] and investments ([Investment])
                
              -- let investmentAmounts = (^. invAmount) <$> getPsInvestments ps in -- [Integer]                
              
              -- Iterate through filteredTransactions, and for each TxHash, call _transactionOutputAmount to get [Amount] 
              -- Then filter getPsInvestments by that TxHash and get the AssetClass, and then extract the TokenName 
              -- Then filter the [Amount] by TokenName, and for the matching Amount for that TokenName, compare the Integer and the Amount
              
                    --  let amountss = _transactionOutputAmount <$> filteredTransactions in -- :: [[Amount]]
                -- let sortedAmountss = 
                -- for each TxHash, is the investmentAmount the same as what we get from blockfrost?
                -- to compare [Integer] to [[Amount]] I can sort each by TxId and zipWith (compare `on` tokenName)
                -- will have to figure out whether it's better to sort first and zipWith or
                -- to not sort and just search through the whole [[Amount]] for each TxHash.
                -- probably easier to do this w/o sorting first and fix it later.
                -- search by TxHash --> TokenName --> Amount == Integer ?
                -- actually before the line above defining amountss, I can start my searching logic there

                -- foldr (\amts z -> foldr (\amt z -> if compareAssets amt ps then z else amt : z) z amts) [] amtss
-- now if the returned list is non-empty, we have transactions that don't match 

-- we extract the TokenName from the AssetClass like so: 
-- newtype AssetClass = AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }

-- a list comprehension may be a good replacement for the fold in main. meditating on it
-- [atxh | itxhs <- itxhss, atxh <- _addressTransactionHash <$> treasuryTransactions, atxh `elem` itxhs]
>>>>>>> parent of 6e584dc (Added logic for comparing AssetClass & Amount):src/Tokenomia/PrivateSale.hs
