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
import qualified Money.SomeDiscrete   as SD 
import qualified Blockfrost.Client    as B

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

getPsInvestments :: PrivateSale -> [Investment]
getPsInvestments ps = investments 

  where
    investments :: [Investment]
    investments = investmentss ^.. folded . folded

    investmentss :: [[Investment]]
    investmentss = (^. piInvestments) <$> investors

    investors :: [PrivateInvestor]
    investors = ps ^. psInvestors

-- TODO: make compareAssets total, i.e. have support for AdaAmount
-- TODO: make types/logic work (comparing lists to non-lists), i.e. the fold in main

-- TODO: BS.empty is the representation of Ada in plutus, but probably need to change for blockfrost
--   to something like in Blockfrost.Types.Shared.Ada: type Lovelaces = Discrete "ADA" "lovelace"

-- TODO: maybe change to :: [Amount] -> PrivateSale -> Bool ?
compareAssets :: Amount -> PrivateSale -> Bool
compareAssets (AdaAmount ll) ps = tokenNames == BS.empty && llAmount == amounts
compareAssets (AssetAmount sd) ps = sdTokenName == tokenNames && sdAmount == amounts

  where -- TODO: define llAmount
    tokenNames :: [TokenName]
    tokenNames = getTokenName <$> assetClasses

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


-- TODO: pass prj into all blockfrost API fxns
verifyPrivateSale
 :: (MonadIO m
   , MonadReader Environment m
   , MonadError TokenomiaError m)
   => m ()
verifyPrivateSale = do
  putStrLn "Please enter the filepath to a private sale .json file"
  jsonFilePath <- getLine
  fileContents <- BS.readFile jsonFilePath
  case (decode fileContents :: Maybe PrivateSale) of
    Nothing -> putStrLn "Unable to parse JSON file"
    Just ps -> do 
      prj <- B.projectFromEnv''
      treasAddrTxs <- liftIO (B.runBlockfrost prj (B.getAddressTransactions . psAddress $ ps)) -- :: [AddressTransaction]
                                  >>= (\case 
                                          Left e    -> tryError $ BlockfrostError e
                                          Right res -> return res)  
                     
        let invs = getPsInvestments ps in      -- :: [Investment]
          let invTxhs = (^. invTx) <$> invs in -- :: [TxHash]
            let txhs = filter (`elem` invTxhs) (_addressTransactionTxHash <$> treasAddrTxs) in -- :: [TxHash]
              case (compare `on` length) txhs invTxhs of -- TODO: is comparing the length enough to make sure
                                                         --   all the TXs are there? seems likely
                LT -> throwError $ BlockFrostError "failed, missing TXs"
                GT -> throwError $ BlockFrostError "this line can only execute if blockfrost provides duplicate TXs"

                EQ -> foldl (\z txh -> 
                  let inv = getInvByTxHash txh invs in                      -- :: Investment (the one that matches the TxHash)
                    let tokenName = getTokenName $ inv ^. invAssetClass in  -- :: TokenName
                      let amount = inv ^. invAmount in do
                        bfTx <- liftIO (B.runBlockfrost prj getTx txh)      -- TODO: fix this line w/ some (.) and ($)
                                  >>= (\case
                                          Left e -> tryError $ BlockfrostError e
                                          Right res -> return res)          -- should be :: Transaction
                                          -- TODO: get [Amount] from Transaction and compare to tokenName and amount above
                          let bfAmts = _transactionOutputAmount bftx in      -- :: [Amount]
                            -- TODO: now filter bfAmounts by Amounts where the amount's tokenName == tokenName above
                            -- TODO: below, make sure types of tokenName and someDiscreteCurrency match
                            let matchingAmt = head . filter (\bfAmt -> SD.someDiscreteCurrency bfAmt == tokenName) bfAmts in
                              case SD.someDiscreteAmount matchingAmt of 
                                amount -> return "success"
                                _      -> tryError $ BlockfrostError e
                              SD.someDiscreteAmount matchingAmt == amount

                            -- getInvByTxHash :: TxHash -> [Investment] -> Investment
                            -- getInvByTxHash txh = head . filter (\inv -> (inv ^. invTx) == txh)

                        -- let bfTx = getTx txh in      
                          


                        -- bfTx :: m Transaction  (this line needs some work, will need to pass prj)



                          -- let bfAmount = 
 
                          -- head . filter (== tokenName) (_transactionOutputAmount txh)

                          -- compareAssets :: Amount -> PrivateSale -> Bool


                          compareAssets ( txh)
                          -- in blockfrost, do: TxHash --> Transaction --> Amount --> someDiscreteAmount, 
                          -- someDiscreteCurrency
                  -- now we have the Investment that corresponds to this TxHash, so get the AssetClass and Integer from Investment

                  ) z txhs


-- TODO: probably don't need to use `on`, just compare length instead. no need to have a GT line when it won't execute anyway
-- TODO: decide if the fold that returns a list of transactions that didn't pass our comparison tests
-- TODO: compare return type of _transactionOutputAmount, i.e. [Amount] to Tx outputs
--   i.e. what we get from blockfrost for a given TxHash is [Amount]. But in the PrivateSale type, each Investment
--   has only 1 Amount (Integer) for each TxHash. Then we check if Integer <= sum $ [Amount] ... I think?
--   Or because Amount == AssetClass, maybe search [Amount] by TokenName, and for the Amount with matching
--   TokenName, make sure the Amount is the same
--   So... get TokenName first, then search through [[Amount]]
--   So I need to make use of these 2 fxn's used earlier:
-- TODO: use nonEmpty instead of unsafely using head?
-- TODO: look into blockfrost lenses

getTokenName :: AssetClass -> TokenName
getTokenName = snd . unAssetClass

assetClasses :: [AssetClass]
assetClasses = (^. invAssetClass) <$> getPsInvestments ps 

getInvByTxHash :: TxHash -> [Investment] -> Investment
getInvByTxHash txh = head . filter (\inv -> (inv ^. invTx) == txh)
                
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
