{-# LANGUAGE RecordWildCards #-}
module Tokenomia.Wallet.Type
    ( WalletName
    , Wallet (..)) where

import           Data.Coerce
import           Tokenomia.Common.Address
import           Tokenomia.Common.Shell.InteractiveMenu


type WalletName = String

data Wallet = Wallet
              { name :: WalletName
              , stakeAddress :: Address} deriving Eq


instance Show Wallet where
    show Wallet {..} = "|| " <> name
        <> " \n | Stake Address :" <> coerce stakeAddress
        -- <> " \n | Child Addresses :" <> (show . size) addresses


instance DisplayMenuItem Wallet where
    displayMenuItem Wallet {..} = name

