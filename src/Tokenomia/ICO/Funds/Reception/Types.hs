{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
module Tokenomia.ICO.Funds.Reception.Types
    ( RoundSettings (..)
    -- Address
    , AddressRef (..)
    , AddressGenerationIndex (..)
    , AddressVolumes (..)
    , AddressFundsState (..)
    , FundsTransaction (..)
    , AddressFundsPlan (..)
    , RejectReason (..)
    , Command (..)
    , NativeTokens
    , Funds
    , ) where

import Prelude hiding (round,print)



import Data.List (intersperse)
import Data.Set.Ordered
import           Plutus.V1.Ledger.Ada

import           Tokenomia.Adapter.Cardano.CLI.Transaction

import           Tokenomia.Adapter.Cardano.CLI.UTxO
import Ledger.Ada as Ada
import qualified Data.Text as T

import Blockfrost.Types.Shared.Amount
import Data.Foldable
import Blockfrost.Pretty.Ada
import Ledger ( Slot(..), TxOutRef(..) )
import Data.Coerce
import qualified Money
import           Plutus.V1.Ledger.Interval

import           Data.Text.Prettyprint.Doc (pretty)


data RoundSettings
        = RoundSettings
          { timeRange :: Interval Slot
          , maximumAdaPerAdress :: Ada
          , fundRange :: Interval Ada}

instance Show RoundSettings where
    show RoundSettings { .. }
        =  "\n | time range  = " <> (show . pretty) timeRange
        <> "\n | fund range = "  <> (show . pretty) fundRange


newtype AddressGenerationIndex = AddressGenerationIndex Integer

data AddressVolumes = AddressVolumes
                { received :: Ada
                , sent :: Ada}

data AddressRef = AddressRef {generationIndex :: AddressGenerationIndex, address :: Address}

instance Show AddressRef where
    show AddressRef {generationIndex = AddressGenerationIndex x,..} 
        = show x <> "#" <> coerce address

instance Show AddressVolumes where
    show AddressVolumes {received = Lovelace x, sent = Lovelace y }
        =  "\n | received = " <> T.unpack (prettyLovelaces (fromIntegral x))
        <> "\n | sent     = " <> T.unpack (prettyLovelaces (fromIntegral y))
        <> "\n | balance  = " <> T.unpack (prettyLovelaces (fromIntegral $ x-y))

data AddressFundsState = AddressFundsState {references :: AddressRef , volumes :: AddressVolumes, transactions :: OSet FundsTransaction} 

data AddressFundsPlan = Plan {references :: AddressRef , commands :: OSet Command} 

type Funds = Either NativeTokens Ada
type NativeTokens = [Amount]

data FundsTransaction
        = FundsTransaction
            { txOutRef :: TxOutRef
            , inputAddresses :: [Address]
            , funds :: Funds
            , transactionSlot :: Slot } deriving Eq

instance Ord FundsTransaction where 
    compare x y = compare (transactionSlot x) (transactionSlot y)

instance Show FundsTransaction where
    show FundsTransaction {..}
        =    "\n | txOutRef       = " <> showTxOutRef txOutRef
        <>   "\n | inputAddresses = " <> fold (intersperse "," (coerce <$> inputAddresses))
        <>   "\n | funds          = " <> fold (intersperse ("\n" <> replicate 20 ' ') (showFunds funds))
        <>   "\n | TxSlot         = " <> (show . getSlot) transactionSlot

     where 
        showFunds :: Funds -> [String]
        showFunds (Right (Ada.Lovelace x)) = [T.unpack (prettyLovelaces $ fromIntegral x)]
        showFunds (Left xs) = showAmount <$> xs

        showAmount :: Amount -> String
        showAmount (AdaAmount x)   = T.unpack (prettyLovelaces x)
        showAmount (AssetAmount y) = ( show . Money.someDiscreteAmount) y <> " " <> (T.unpack . Money.someDiscreteCurrency) y


data RejectReason
    = AddressSaturated
    | FundsWithNativeTokens
    | TransactionOutofRoundTimeRange
    | InsufficientFundsReceived
    deriving (Show,Eq)


data Command
    = Reject {transaction :: FundsTransaction, reason :: RejectReason}
    | AcceptWithPartialRefund {transaction :: FundsTransaction, refundAmount :: Ada}
    | Accept {transaction :: FundsTransaction} deriving Eq

instance Ord Command where 
    compare x y = compare (transaction x) (transaction y)

instance Show Command where
    show Accept {transaction = FundsTransaction {..}} 
        =  show (getSlot transactionSlot) 
            <> " - " 
            <>  showTxOutRef txOutRef <> " : Accept"
    show Reject {transaction = FundsTransaction {..}, ..} 
        =  show (getSlot transactionSlot) 
            <> " - " 
            <> showTxOutRef txOutRef 
            <> " : Reject (" <> show reason <> ")"
    show AcceptWithPartialRefund {transaction = FundsTransaction {..}, ..}
        =  show (getSlot transactionSlot) 
            <> " - " 
            <> showTxOutRef txOutRef 
            <> " : Partially Accept ( refund of " <> show (prettyLovelaces $ fromIntegral refundAmount) <> ")"

