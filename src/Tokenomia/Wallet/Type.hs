{-# LANGUAGE DerivingStrategies                        #-}
{-# LANGUAGE RecordWildCards                           #-}
module Tokenomia.Wallet.Type
    ( Wallet(..)
    , WalletName
    ) where

import Data.Coerce                                     ( coerce )
import Tokenomia.Common.Address                        ( Address(..) )
import Tokenomia.Common.Shell.InteractiveMenu          ( DisplayMenuItem(..) )


type WalletName = String

data Wallet = Wallet
              { name :: WalletName
              , stakeAddress :: Address} deriving stock Eq


instance Show Wallet where
    show Wallet {..} = "|| " <> name
        <> " \n | Stake Address :" <> coerce stakeAddress
        -- <> " \n | Child Addresses :" <> (show . size) addresses


instance DisplayMenuItem Wallet where
    displayMenuItem Wallet {..} = name

