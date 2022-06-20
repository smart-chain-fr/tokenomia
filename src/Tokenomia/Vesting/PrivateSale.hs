{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE BlockArguments      #-}

module Tokenomia.Vesting.PrivateSale (verifyPrivateSale) where

import           Prelude                (IO, Semigroup (..), Show (..), String)
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Value           as Value
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Ada

import           GHC.Generics           (Generic)
import           Control.Monad          hiding (fmap)
import           Control.Monad.Reader   hiding (ask)
import           Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import           Control.Monad.Except
import           Control.Lens 
import           System.IO                  (FilePath, putStrLn, getLine)
import           Data.Aeson                 (FromJSON, decode)
import           Data.Text                  (Text, pack)
import           Data.Time                  (UTCTime(UTCTime))
import           Data.Function              (on)
import           Data.Foldable              (foldr1)
import           Data.List                  (sort)
import           Data.ByteString.Lazy       (readFile)
import           Data.ByteString.Internal   (unpackChars)
import           Money                

import qualified Blockfrost.Client      as B
import qualified Blockfrost.Lens        as B

import           Tokenomia.Common.Error
import           Tokenomia.Common.Environment
import           Tokenomia.Common.Blockfrost (projectFromEnv'')

instance FromJSON PrivateSale
instance FromJSON PrivateInvestor 
instance FromJSON Investment 
instance FromJSON Tranche 

type Percentage = Integer 
type Duration   = Integer 

instance Eq Text where 
  x == y = True

instance Eq B.TxHash where
  x == y = True

data PrivateSale
  = PrivateSale
    { _psAddress         :: B.Address    -- Treasury address
    , _psStart           :: POSIXTime
    , _psTranches        :: [Tranche]
    , _psAssetClass      :: AssetClass   -- Vesting token
    , _psInvestors       :: [PrivateInvestor]
    } deriving (Generic, Show)

data Tranche 
  = Tranche
    { _tranchePercentage :: Percentage
    , _trancheDuration   :: Duration 
    } deriving (Generic, Show)

data PrivateInvestor
  = PrivateInvestor
    { _piAddress         :: B.Address
    , _piAllocation      :: Integer -- Amount of vesting tokens to lock (note, this is likely NOT equal to the total amounts of investments, as different tokens)
    , _piInvestments     :: [Investment]
    } deriving (Generic, Show)

data Investment
  = Investment
    { _invTx             :: B.TxHash    -- Tx that sends below asset class to payment address
    , _invAssetClass     :: AssetClass
    , _invAmount         :: Integer -- Amount of above asset class expected to be sent to payment address
    } deriving (Generic, Show)

makeLenses ''PrivateSale
makeLenses ''Tranche
makeLenses ''PrivateInvestor
makeLenses ''Investment

verifyPrivateSale :: 
  (MonadIO m
  , MonadError  TokenomiaError m
  , MonadReader Environment m)
  => m ()
verifyPrivateSale = do
  liftIO . putStrLn $ "Please enter a filepath with JSON data"
  jsonFilePath <- liftIO getLine
  verifyPrivateSale' jsonFilePath

verifyPrivateSale' :: 
  (MonadIO m
  , MonadError  TokenomiaError m
  , MonadReader Environment m)
  => FilePath 
  -> m ()
verifyPrivateSale' jsonFilePath = do
  ps <- jsonToPrivateSale jsonFilePath
  treasAddrTxs <- getTreasAddrTxs ps               -- :: [B.AddressTransaction] <- m [B.AddressTransaction] 
  let invTxhs = privateSaleToTxhs ps in
    let txhs = verifyTxHashList invTxhs treasAddrTxs in
      if length txhs == length invTxhs then do
        verifyTxs (getPsInvestments ps) txhs
        else throwError . BlockFrostError . B.BlockfrostError $ "Unequal lengths, means missing TXs"

jsonToPrivateSale ::
  (MonadIO m
  , MonadError  TokenomiaError m
  , MonadReader Environment m)
  => FilePath 
  -> m PrivateSale
jsonToPrivateSale jsonFilePath = do
    fileContents <- liftIO . readFile $ jsonFilePath
    case (decode fileContents :: Maybe PrivateSale) of
        Nothing -> throwError . BlockFrostError . B.BlockfrostError $ "Unable to parse JSON"
        Just ps -> return ps

privateSaleToTxhs :: PrivateSale -> [B.TxHash]
privateSaleToTxhs ps = invTxhs 

  where
    invTxhs :: [B.TxHash]
    invTxhs = (^. invTx) <$> invs

    invs :: [Investment]
    invs = getPsInvestments ps

verifyTxs ::
  (MonadIO m
  , MonadError  TokenomiaError m
  , MonadReader Environment m)
  => [Investment]
  -> [B.TxHash]
  -> m ()
verifyTxs invs = 
    foldl (\z txh -> do
    bfTx <- lift . getTxByTxHash $ txh                                      -- :: m B.Transaction
    -- inv  <- MaybeT . return $ getInvByTxHash txh invs                    -- :: Maybe Investment
    -- inv <- getInvByTxHash txh invs
    let inv = getInvByTxHash txh invs in
      let invTokenName = getTokenName $ inv ^. invAssetClass                  -- :: Text
          bfAmts = bfTx ^. B.outputAmount                                     -- :: [B.Amount]
          invAmount' = inv ^. invAmount in                                    -- :: Integer
              let amt = getAmountByTokenName invTokenName bfAmts in           -- :: B.Amount
              if getDiscreteAmount amt == invAmount' then z 
              else throwError . BlockFrostError . B.BlockfrostError $ "Amts don't match"
              ) (return ())
        
getTreasAddrTxs ::
  (MonadIO m
  , MonadError  TokenomiaError m
  , MonadReader Environment m)
  => PrivateSale 
  -> m [B.AddressTransaction]
getTreasAddrTxs ps = do
  prj <- projectFromEnv''
  liftIO $ B.runBlockfrost prj $ do
    B.getAddressTransactions (ps ^. psAddress)
  >>= (\case 
      Left e    -> throwError $ BlockFrostError e
      Right res -> return res)

getTxByTxHash ::
  (MonadIO m
  , MonadError  TokenomiaError m
  , MonadReader Environment m)
  => B.TxHash
  -> m B.Transaction
getTxByTxHash txh = do
  prj <- projectFromEnv''
  liftIO $ B.runBlockfrost prj $ do
    B.getTx txh
  >>= (\case
        Left e    -> throwError $ BlockFrostError e
        Right res -> return res)

getTokenName :: AssetClass -> Text
getTokenName = pack . unpackChars . fromBuiltin . unTokenName . snd . unAssetClass
        
--TODO: use nonEmpty so I don't have to have a case for an empty list. then I don't need to return maybe
--   and then I wont need to use MaybeT
-- getInvByTxHash :: B.TxHash -> [Investment] -> Maybe Investment
-- getInvByTxHash _ [] = Nothing
-- getInvByTxHash txh invs = Just . head . filter (\inv -> (inv ^. invTx) == txh) $ invs
--TODO: FIX!!
getInvByTxHash :: B.TxHash -> [Investment] -> Investment
-- getInvByTxHash _ [] = undefined
getInvByTxHash txh invs = head . filter (\inv -> (inv ^. invTx) == txh) $ invs

getPsInvestments :: PrivateSale -> [Investment]
getPsInvestments ps = investments 

  where
    investments  :: [Investment]
    investments = concat ((^. piInvestments) <$> investors)

    investors :: [PrivateInvestor]
    investors = ps ^. psInvestors

verifyTxHashList :: [B.TxHash] -> [B.AddressTransaction] -> [B.TxHash]
verifyTxHashList txhs addrTxs = 
  filter (`elem` txhs) ((^. B.txHash) <$> addrTxs)

-- getAmountByTokenName :: Text -> [B.Amount] -> Maybe B.Amount
getAmountByTokenName :: Text -> [B.Amount] -> B.Amount
getAmountByTokenName tn = foldr1 (\amt z -> if getDiscreteCurrency amt == tn then amt else z)

getDiscreteCurrency :: B.Amount -> Text
getDiscreteCurrency (B.AdaAmount ll)   = "ADA"
getDiscreteCurrency (B.AssetAmount sd) = someDiscreteCurrency sd

getDiscreteAmount :: B.Amount -> Integer
getDiscreteAmount (B.AdaAmount ll)   = someDiscreteAmount . toSomeDiscrete $ ll 
getDiscreteAmount (B.AssetAmount sd) = someDiscreteAmount sd

-- someDiscreteCurrency :: SomeDiscrete -> Text
-- someDiscreteAmount :: SomeDiscrete -> Integer

--    if I have a String type, I can use pack :: String -> Text in Data.Text
  -- newtype CurrencySymbol = CurrencySymbol { unCurrencySymbol :: Builtins.ByteString }
  -- newtype TokenName = TokenName { unTokenName :: Builtins.ByteString }
  -- unpack :: ByteString -> [Char] (this is from https://hackage.haskell.org/package/bytestring-0.10.8.1/docs/Data-ByteString-Lazy-Char8.html#v:unpack)
        

-- newtype AssetClass = AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }

       -- TODO: is comparing the length enough to make sure
       --   all the TXs are there? seems likely
       
      {-
newtype Address = Address Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)
-}



        -- case (compare `on` length) txhs invTxhs of
        --       LT -> throwError $ BlockFrostError "failed, missing TXs"
        --       GT -> throwError $ BlockFrostError "this line can only execute if blockfrost provides duplicate TXs"
        --       EQ -> do verifyTxs invs txhs



        -- getAmountByTokenName :: Text -> B.Amount -> Maybe B.Amount
-- getAmountByTokenName "ADA" amt@(B.AdaAmount _) = Just amt
-- getAmountByTokenName _ (B.AdaAmount _)         = Nothing
-- getAmountByTokenName tokenName amt@(B.AssetAmount sd) = 
--   if someDiscreteCurrency sd == tokenName then Just amt else Nothing




-- jsonToPrivateSale :: FilePath -> IO PrivateSale
-- jsonToPrivateSale jsonFilePath = do 
--   fileContents <- readFile jsonFilePath       -- :: ByteString <- IO ByteString      
--   case (decode fileContents :: Maybe PrivateSale) of
--       Nothing -> throwError "Unable to parse JSON"
--       Just ps -> return ps



  -- let invs = getPsInvestments ps in                -- :: [Investment]
  --   let invTxhs = (^. invTx) <$> invs in           -- :: [TxHash]


    -- ps <- liftIO . jsonToPrivateSale $ jsonFilePath  -- :: PrivateSale <- IO PrivateSale                      -- MaybeT IO Bool

    -- jsonToPrivateSale :: FilePath -> IO PrivateSale


      -- ps <- jsonToPrivateSale jsonFilePath                
  -- treasAddrTxs <- getTreasAddrTxs ps               -- :: [B.AddressTransaction] <- m [B.AddressTransaction] 
  -- let invTxhs = privateSaleToTxhs ps in
  --   let txhs = verifyTxHashList invTxhs treasAddrTxs in
  --     if length txhs == length invTxhs then do
  --       verifyTxs (getPsInvestments ps) txhs
  --       else throwError . BlockFrostError . B.BlockfrostError $ "Unequal lengths, means missing TXs"



    --TODO: get rid of MaybeT in verifyTxs and return m ()
-- verifyTxs ::
--   (MonadIO m
--   , MonadError  TokenomiaError m
--   , MonadReader Environment m)
--   => [Investment]
--   -> [B.TxHash]
--   -> MaybeT m ()
-- verifyTxs ::
--   (MonadIO m
--   , MonadError  TokenomiaError m
--   , MonadReader Environment m)
--   => [Investment]
--   -> [B.TxHash]
--   -> m ()
-- verifyTxs invs = 
--   foldl (\z txh -> do
--     bfTx <- lift . getTxByTxHash $ txh                                      -- :: m B.Transaction
--     inv  <- MaybeT . return $ getInvByTxHash txh invs                       -- :: Maybe Investment
--     let invTokenName = getTokenName $ inv ^. invAssetClass                  -- :: Text
--         bfAmts = bfTx ^. B.outputAmount                                     -- :: [B.Amount]
--         invAmount' = inv ^. invAmount in                                    -- :: Integer
--             let amt = getAmountByTokenName invTokenName bfAmts in           -- :: B.Amount
--             if getDiscreteAmount amt == invAmount' then z else throwError . BlockFrostError . B.BlockfrostError $ "Amts don't match"
--             ) (MaybeT . return . Just $ ())


-- AdaAmount Lovelaces
-- type Lovelaces = Discrete "ADA" "lovelaces"
-- the chain is: getTx :: Transaction -> [B.Amount] -> Maybe B.Amount -> toSomeDiscrete :: Discrete' currency scale -> SomeDiscrete