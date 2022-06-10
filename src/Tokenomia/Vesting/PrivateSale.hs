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

module PrivateSale (verifyPrivateSale) where

-- TODO: add necessary imports, remove unnecessary imports 
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
import qualified Money.SomeDiscrete   as SD 
import qualified Blockfrost.Client    as B
import qualified Blockfrost.Lens      as B

type Percentage = Integer 
type Duration   = Integer 

instance FromJSON PrivateSale
instance FromJSON PrivateInvestor 
instance FromJSON Investment 
instance FromJSON Tranche 

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
    { _invTx             :: TxHash    -- Tx that sends below asset class to payment address
    , _invAssetClass     :: AssetClass
    , _invAmount         :: Integer -- Amount of above asset class expected to be sent to payment address
    } deriving (Generic, Show)

makeLenses ''PrivateSale
makeLenses ''Tranche
makeLenses ''PrivateInvestor
makeLenses ''Investment

-- TODO: pass relevant vals as params to verifyPrivateSale, instead of binding everything inline
-- TODO: fix types for verifyPrivateSale
verifyPrivateSale
 :: (MonadIO m
   , MonadReader Environment m
   , MonadError TokenomiaError m)
   => m Bool
verifyPrivateSale = do
  putStrLn "Please enter the filepath to a private sale .json file"
  jsonFilePath <- getLine
  fileContents <- BS.readFile jsonFilePath
  case (decode fileContents :: Maybe PrivateSale) of
    Nothing -> throwError $ BlockFrostError "Unable to parse JSON"
    Just ps -> do 
      prj <- B.projectFromEnv''
      -- treasAddrTxs :: Either BlockfrostError [AddressTransaction]
      treasAddrTxs <- liftIO (B.runBlockfrost prj (B.getAddressTransactions . psAddress $ ps))
      case treasAddrTxs of 
        Left e              -> throwError $ BlockFrostError e
        Right treasAddrTxs' ->                   -- treasAddrTxs' :: [AddressTransaction]
          let invs = getPsInvestments ps in       -- :: [Investment]
            let invTxhs = (^. invTx) <$> invs    -- :: [TxHash]
                txhs = verifyTxHashList invTxhs treasAddrTxs' in
                  case (compare `on` length) txhs invTxhs of -- TODO: is comparing the length enough to make sure
                                                             --   all the TXs are there? seems likely
                    LT -> throwError $ BlockFrostError "failed, missing TXs"
                    GT -> throwError $ BlockFrostError "this line can only execute if blockfrost provides duplicate TXs"
                  -- note: the GT line above means that the only way the [TxHash] from the investment address can be greater than
                  -- invTxhs after filtering with verifyTxHashList is if we have duplicate transactions, so probably 
                  -- a good idea to account for that possibility

                    EQ -> 
                      let txsAreValid = foldl (\z txh -> do
                          bfTx <- liftIO (B.runBlockfrost prj (B.getTx txh))     -- bfTx :: Either BlockfrostError B.Transaction
                          inv  <- getInvByTxHash txh invs                         -- :: Just Investment (the one that matches the TxHash)
                          let invTokenName = getTokenName $ inv ^. invAssetClass    -- :: Text
                              invAmount' = inv ^. invAmount in                       -- :: Integer
                                case bfTx of 
                                  Left e      -> throwError $ BlockFrostError e
                                  Right bfTx' ->                                      -- bfTx' :: B.Transaction
                                    let bfAmts = bfTx' ^. outputAmount in                   -- :: [B.Amount]
                                      case head (getAmountByTokenName invTokenName <$> bfAmts) of
                                        Nothing  -> throwError $ BlockFrostError "No matching Amount for the given tokenName"
                                        Just amt -> return ((getDiscreteAmount amt == invAmount') && z)
                          ) True txhs in
                        return txsAreValid
                      return txsAreValid

getPsInvestments :: PrivateSale -> [Investment]
getPsInvestments ps = investments 

  where
    investments  :: [Investment]
    investments = investmentss ^.. folded . folded

    investmentss :: [[Investment]]
    investmentss = (^. piInvestments) <$> investors

    investors    :: [PrivateInvestor]
    investors = ps ^. psInvestors

verifyTxHashList :: [TxHash] -> [AddressTransaction] -> [TxHash]
verifyTxHashList txhs addrTxs = 
  filter (`elem` txhs) ((^. txHash) <$> addrTxs)

getAmountByTokenName :: Text -> B.Amount -> Maybe B.Amount
getAmountByTokenName "ADA" amt@(AdaAmount _) = Just amt
getAmountByTokenName _ (AdaAmount _)         = Nothing
getAmountByTokenName tokenName amt@(AssetAmount sd) = 
  if getDiscreteCurrency sd == tokenName then Just amt else Nothing

-- type Lovelaces = Discrete "ADA" "lovelace"

-- gets the TokenName that corresponds to the B.Amount (where B.Amount is like AssetClass)
getDiscreteCurrency :: B.Amount -> Text
getDiscreteCurrency (AdaAmount ll)   = "ADA"
getDiscreteCurrency (AssetAmount sd) = SD.someDiscreteCurrency amt

-- TODO: must use discreteToDecimal :: GoodScale scale => DecimalConf -> Approximation -> Discrete' currency scale -> Text
--      in order to pull out the AdaAmount
-- gets the value of the transaction for that AssetClass (Amount in blockfrost is like both AssetClass and # of tokens in a Tx)
getDiscreteAmount :: B.Amount -> Integer
getDiscreteAmount (AdaAmount ll)   = undefined
getDiscreteAmount (AssetAmount sd) = SD.someDiscreteAmount sd

-- someDiscreteCurrency :: SomeDiscrete -> Text
-- someDiscreteAmount :: SomeDiscrete -> Integer

--    if I have a String type, I can use pack :: String -> Text in Data.Text
  -- newtype CurrencySymbol = CurrencySymbol { unCurrencySymbol :: Builtins.ByteString }
  -- newtype TokenName = TokenName { unTokenName :: Builtins.ByteString }
  -- unpack :: ByteString -> [Char] (this is from https://hackage.haskell.org/package/bytestring-0.10.8.1/docs/Data-ByteString-Lazy-Char8.html#v:unpack)
  --   so make sure this actually works with the ByteString wrapped in TokenName / CurrencySymbol newtypes
  -- for now I'll rewrite this fxn to return a Text type, might change back to returning TokenName in the future,
  -- and abstract the logic to convert to Text into another fxn.
  -- note: i believe there's more necessary conversion between Word8 and Char8 RE pack and unpack below. 
getTokenName :: AssetClass -> Text
getTokenName = Data.Text.pack . Data.ByteString.Lazy.Char8.unpack . unTokenName . snd . unAssetClass

assetClasses :: [AssetClass]
assetClasses = (^. invAssetClass) <$> getPsInvestments ps 

-- TODO: add support for Maybe
getInvByTxHash :: TxHash -> [Investment] -> Maybe Investment
getInvByTxHash _ [] = Nothing
getInvByTxHash txh invs = Just (head . filter (\inv -> (inv ^. invTx) == txh) invs)           

-- newtype AssetClass = AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }

-- a list comprehension may be a good replacement for the fold in main. meditating on it
-- [atxh | itxhs <- itxhss, atxh <- _addressTransactionHash <$> treasuryTransactions, atxh `elem` itxhs]
