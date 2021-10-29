{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Tokenomia.Vesting.Repository
    ( register
    , WalletWithVestedFunds (..)
    , Vesting (..)
    , VestingContext (..)
    , VestingState (..)
    , TrancheContext (..)
    , TrancheState (..)
    , selectWalletWithVestedFunds
    , selectVesting
    ) where

import           Data.Aeson hiding (Value)

import           System.Directory

import           Tokenomia.Adapter.Cardano.CLI.Folder (getFolderPath,Folder (..))
import           Prelude hiding ((+),(-))
import           Data.Maybe
import qualified Data.Text as T
import           Data.List.NonEmpty hiding (filter,length,map)
import qualified Data.Time.Clock.POSIX as POSIX

import           Control.Monad.Reader


import           PlutusTx.Prelude  (AdditiveSemigroup((+)))
import           Ledger.Value
import           Ledger hiding (singleton,Address)


import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO

import           Tokenomia.Vesting.Contract
import           Tokenomia.Common.Shell.InteractiveMenu
import           Tokenomia.Adapter.Cardano.CLI.Environment


import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Adapter.Cardano.CLI.Scripts

import qualified Tokenomia.Adapter.Cardano.CLI.UTxO.Query as UTxOs


data WalletWithVestedFunds = WalletWithVestedFunds {wallet :: Wallet, vestedFunds :: NonEmpty Vesting}
data Vesting = Vesting VestingContext VestingState deriving (Eq,Show) 

data VestingContext
    = VestingContext
    { scriptLocation :: ScriptLocation
    , validator :: Validator
    , vestingParam :: VestingParams
    , investorId :: PubKeyHash
    , tranches :: (TrancheContext,TrancheContext)
    } deriving (Eq,Show)

data TrancheContext = TrancheContext {deadline :: POSIXTime, valueVested :: Value} deriving (Eq,Show) 

data VestingState 
    = VestingState 
    { utxosOnScript :: [UTxO]
    , tranches :: (TrancheState,TrancheState)
    } deriving (Eq,Show) 

data TrancheState
    = Locked 
    | Available 
    | Retrieved deriving (Eq,Show)


selectWalletWithVestedFunds :: (MonadIO m , MonadReader Environment m)=> m (Maybe WalletWithVestedFunds)
selectWalletWithVestedFunds = 
    getWalletWithVestedFunds
      >>=  \case
            Nothing -> return Nothing
            Just a -> Just <$> showMenu a
  where
  showMenu :: (MonadIO m) => NonEmpty WalletWithVestedFunds -> m WalletWithVestedFunds
  showMenu wallets = liftIO $ askMenu wallets

instance DisplayMenuItem WalletWithVestedFunds where
    displayMenuItem WalletWithVestedFunds{wallet = Wallet{..},..}
        = name <> " (" <> (show . length ) vestedFunds <> " vesting script(s))"


getWalletWithVestedFunds :: (MonadIO m , MonadReader Environment m) => m (Maybe (NonEmpty WalletWithVestedFunds))
getWalletWithVestedFunds = 
    getVestingInProgress
     >>= \case
            Nothing -> return Nothing 
            Just allVestingInProgress -> do 
                query_registered_wallets
                 >>= return 
                     . nonEmpty 
                     . catMaybes 
                     . map (\wallet@Wallet {publicKeyHash} -> 
                            let vestingForWallet = filter (\(Vesting VestingContext{investorId} _) -> investorId == publicKeyHash ) . toList $ allVestingInProgress
                            in case nonEmpty vestingForWallet of 
                                    Nothing -> Nothing 
                                    Just vestedFunds -> Just WalletWithVestedFunds {..})


selectVesting :: (MonadIO m) => NonEmpty Vesting -> m Vesting
selectVesting  =  askMenu 


getVestingInProgress :: (MonadIO m, MonadReader Environment m)  => m (Maybe (NonEmpty Vesting))
getVestingInProgress  = do
    now <- convertToInternalPosix <$> liftIO POSIX.getPOSIXTime
    vestingParams <- getAll
    nonEmpty . filter isInProgress <$> mapM (convert now) vestingParams
    where
        convert :: (MonadIO m, MonadReader Environment m)  => POSIXTime ->  VestingParams -> m Vesting
        convert now vestingParam@VestingParams
            {  vestingTranche1 =
                    VestingTranche
                    { vestingTrancheDate   = deadline1
                    , vestingTrancheAmount = token1}
            ,  vestingTranche2 =
                    VestingTranche
                    { vestingTrancheDate   = deadline2
                    , vestingTrancheAmount = token2}
            , vestingOwner = investorId } = do
          let validator = vestingScript vestingParam
          scriptLocation <- getScriptLocation validator
          (utxosOnScript,retrieveState) <- UTxOs.query (onChain scriptLocation)
                                                >>= \case
                                                    [] -> return ([],AllTranchesRetrieved)
                                                    utxosOnScript@[_] -> return (utxosOnScript,AllTranchesRetrieved)
                                                    utxosOnScript@[_,_] -> return (utxosOnScript,ZeroTrancheRetrieved)
                                                    _ ->  error $ "retrieved an unexpected number of UTXos at " <> onChain scriptLocation
          let (s1,s2) = getTrancheStates now deadline1 deadline2 retrieveState
          return
            (Vesting 
                VestingContext {tranches = 
                                (TrancheContext deadline1 token1
                                ,TrancheContext deadline2 token2)
                                , ..}
                VestingState {tranches = (s1,s2), ..})

isInProgress :: Vesting -> Bool 
isInProgress (Vesting _ VestingState {..}) 
    = case tranches of 
        (Retrieved,Retrieved) -> False 
        _ -> True 


getTrancheStates :: POSIXTime -> POSIXTime -> POSIXTime -> RetrieveState -> (TrancheState,TrancheState) 
getTrancheStates now d1 d2 state = 
    case (reached d1, reached d2, state) of 
        (False ,False,ZeroTrancheRetrieved) -> (Locked   , Locked)
        (False ,False,OneTrancheRetrieved) -> error "unexpected State : 2 Tranches Locked and One Tranche Retrieved " 
        (True,False,ZeroTrancheRetrieved)  -> (Available , Locked )
        (True,False,OneTrancheRetrieved)   -> (Retrieved , Locked )
        (False,True,ZeroTrancheRetrieved)  -> (Locked    , Available )
        (False,True,OneTrancheRetrieved)   -> (Locked    , Retrieved )
        (True,True,ZeroTrancheRetrieved)   -> (Available , Available)
        (True,True,OneTrancheRetrieved)    -> (Retrieved , Available) -- by convention we retrieve tranche 1 firstly
        (_,_,AllTranchesRetrieved)         -> (Retrieved , Retrieved)      
  where 
    reached = (< now)

data RetrieveState = OneTrancheRetrieved | ZeroTrancheRetrieved | AllTranchesRetrieved
       


register :: ( MonadIO m , MonadReader Environment m) => VestingParams -> m ()
register vestingParams = do
    indexFolder <- getFolderPath Validators
    let filePath = indexFolder <> "vesting.index"
    liftIO (doesFileExist filePath)
        >>= \case
             False -> liftIO $ encodeFile filePath [vestingParams]
             True ->  liftIO $ decodeFileStrict @[VestingParams] filePath
                        >>= \case
                            Nothing -> error "Vesting index is badly formed"
                            Just params -> liftIO $ encodeFile filePath (vestingParams:params)


getAll :: ( MonadIO m , MonadReader Environment m) => m [VestingParams]
getAll = do
    indexFolder <- getFolderPath Validators
    let filePath = indexFolder <> "vesting.index"
    liftIO (doesFileExist filePath)
        >>= \case
             False -> return []
             True ->  liftIO $ decodeFileStrict @[VestingParams] filePath
                >>= \case
                    Nothing -> error "Vesting index is badly formed"
                    Just params -> return params


instance DisplayMenuItem Vesting where
    displayMenuItem (Vesting 
                        VestingContext { tranches = ( TrancheContext {deadline = d1, valueVested = v1}
                                                    , TrancheContext {deadline = d2, valueVested = v2})}
                        VestingState { tranches = (s1,s2)})
        = case (s1,s2) of
            (Available ,Available)   -> "Funds available :" <> "\n\t   * " <> (T.unpack . toCLI) (v1 + v2)
            (Retrieved,Retrieved)    -> "Funds totally retrieved"
            (Locked,Available)       -> "Funds Partially Unlocked :"
                                            <> "\n\t   * " <> (T.unpack . toCLI) v1  <> "(locked till " <> (formatISO8601 . convertToExternalPosix) d1 <> ")"
                                            <> "\n\t   * " <> (T.unpack . toCLI) v2  <> "(available)"         
            (Available,Locked)   -> "Funds Partially Unlocked :"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v1  <> "(available)"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v2  <> "(locked till " <> (formatISO8601 . convertToExternalPosix) d2 <> ")"
            (Locked ,Locked )    -> "Funds Fully Locked :"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v1  <> "(locked till " <> (formatISO8601 . convertToExternalPosix) d1 <> ")"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v2  <> "(locked till " <> (formatISO8601 . convertToExternalPosix) d2 <> ")"
            (Retrieved ,Locked ) -> "Funds Partially Locked :"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v1  <> "(retrieved)"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v2  <> "(locked till " <> (formatISO8601 . convertToExternalPosix) d2 <> ")"
            (Retrieved,Available) -> "Funds Partially Locked :"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v1  <> "(retrieved)"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v2  <> "(available)"
            (Locked,Retrieved)    -> "Funds Partially Locked :"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v1  <> "(locked till " <> (formatISO8601 . convertToExternalPosix) d1 <> ")"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v2  <> "(retrieved)"
            (Available,Retrieved) -> "Funds Partially Locked :"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v1  <> "(available)"                           
                                    <> "\n\t   * " <> (T.unpack . toCLI) v2  <> "(retrieved)"

