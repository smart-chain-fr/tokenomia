{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Tokenomia.ICO.Funds.Validation.ChildAddress.Types
    ( WhiteListedInvestorRef (..)
    , WhiteListedInvestorState (..)
    , AddressVolumes (..)
    , ReceivedFunds (..)
    , NativeTokens
    , Funds
    , getIndex
    , getAdas
    , volumesMatchesFundsReceived
    , isNativeTokenFund
    , ) where

import           Prelude hiding (round,print)

import           Data.List (intersperse)
import Data.Set.Ordered ( OSet, toAscList )
import Plutus.V1.Ledger.Ada ( Ada(Lovelace) )


import Ledger.Ada as Ada ( Ada(Lovelace) )
import qualified Data.Text as T

import Blockfrost.Types.Shared.Amount ( Amount(..) )
import Data.Foldable ( Foldable(fold) )
import Blockfrost.Pretty.Ada ( prettyLovelaces )
import           Ledger ( Slot(..), TxOutRef(..) )
import qualified Money
import Tokenomia.Common.Address ( Address(..) )
import Tokenomia.Common.TxOutRef ( showTxOutRef )
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
import Data.Coerce  

import Control.Monad.Reader ()


data AddressVolumes = AddressVolumes
                { received :: Ada
                , sent :: Ada} deriving (Eq)


instance Show AddressVolumes where
    show AddressVolumes {received = Lovelace x, sent = Lovelace y }
        =  "\n | received = " <> T.unpack (prettyLovelaces (fromIntegral x))
        <> "\n | sent     = " <> T.unpack (prettyLovelaces (fromIntegral y))
        <> "\n | balance  = " <> T.unpack (prettyLovelaces (fromIntegral $ x-y))


data WhiteListedInvestorRef 
        = WhiteListedInvestorRef 
          { exchangePaybackAddress :: Address
          , indexedAddress         :: IndexedAddress} deriving (Eq)

getIndex :: WhiteListedInvestorRef -> ChildAddressIndex
getIndex = index . childAddressRef . indexedAddress

instance Show WhiteListedInvestorRef where
    show WhiteListedInvestorRef {indexedAddress = IndexedAddress {childAddressRef = ChildAddressRef {..},..}, ..}
        =    "\n> reception location"
        <>   "\n   | address = " <> coerce address
        <>   "\n   | index   = " <> (show @Integer . coerce) index
        <>   "\n> exchange payback address = " <> coerce exchangePaybackAddress


data WhiteListedInvestorState 
    = WhiteListedInvestorState 
        { investorRef :: WhiteListedInvestorRef 
        , volumes :: AddressVolumes
        , allReceivedFunds :: OSet ReceivedFunds} deriving Eq

instance Show WhiteListedInvestorState where
    show WhiteListedInvestorState {..}
        =  "\n || WhiteListedInvestorState || "
        <>  show volumes
        <>  (show . toAscList ) allReceivedFunds
        


type Funds = Either NativeTokens Ada
type NativeTokens = [Amount]

data ReceivedFunds
        = ReceivedFunds
            { txOutRef :: TxOutRef
            , funds :: Funds
            , receivedAt :: Slot } deriving Eq

instance Ord ReceivedFunds where 
    compare x y = case compare (receivedAt x) (receivedAt y) of 
      LT -> LT
      EQ -> compare (txOutRef x) (txOutRef y) 
      GT -> GT

instance Show ReceivedFunds where
    show ReceivedFunds {..}
        =    "\n || ReceivedFunds || "
        <>   "\n | received at Slot = " <> (show . getSlot) receivedAt
        <>   "\n | funds            = " <> fold (intersperse ("\n" <> replicate 22 ' ') (showFunds funds))
        <>   "\n | txOutRef         = " <> showTxOutRef txOutRef

     where 
        showFunds :: Funds -> [String]
        showFunds (Right (Ada.Lovelace x)) = [T.unpack (prettyLovelaces $ fromIntegral x)]
        showFunds (Left xs) = showAmount <$> xs

        showAmount :: Amount -> String
        showAmount (AdaAmount x)   = T.unpack (prettyLovelaces x)
        showAmount (AssetAmount y) = ( show . Money.someDiscreteAmount) y <> " " <> (T.unpack . Money.someDiscreteCurrency) y

volumesMatchesFundsReceived :: WhiteListedInvestorState -> Bool 
volumesMatchesFundsReceived WhiteListedInvestorState {volumes = AddressVolumes {..},..} 
  = received - sent == foldMap getAdas allReceivedFunds

getAdas :: ReceivedFunds -> Ada 
getAdas ReceivedFunds {funds = Right adas} = adas
getAdas ReceivedFunds {funds = Left nativeTokens} = foldMap filterOnlyLovelaces nativeTokens

filterOnlyLovelaces :: Amount -> Ada
filterOnlyLovelaces (AdaAmount x)   = fromIntegral x
filterOnlyLovelaces (AssetAmount _) = 0

isNativeTokenFund :: ReceivedFunds -> Bool 
isNativeTokenFund ReceivedFunds {funds = Left _} = True 
isNativeTokenFund _ = False