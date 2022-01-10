{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.ICO.Funds.WhiteListing.Repository
    ( fetchAllWhiteListedInvestorRef
    , fetchPaybackAddress) where

import Prelude hiding (round,print)
import           Control.Monad.Reader
import           Control.Monad.Except



import           Tokenomia.Common.Environment
import           Tokenomia.Common.Error
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import           Tokenomia.ICO.Funds.Validation.ChildAddress.Types
import           Tokenomia.Common.Address ( Address(..) )
import qualified Data.List.NonEmpty as NEL


-- N.H : TODO 
fetchAllWhiteListedInvestorRef
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
    => NEL.NonEmpty IndexedAddress
    -> m (NEL.NonEmpty WhiteListedInvestorRef)
fetchAllWhiteListedInvestorRef activeAddresses =
    return $ WhiteListedInvestorRef
        (Address "addr_test1qpwaa235rqypsjyy886260gw83m8q0ls7cz5f5n9fwc5lyf2gdnrkx40pwc4jef679xte56jx3jz6mc73t6w7ac53q2qqqnxv8")
            <$> activeAddresses

fetchPaybackAddress
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
    => ChildAddressRef
    -> m Address
fetchPaybackAddress _ =
    return $ Address "addr_test1qpwaa235rqypsjyy886260gw83m8q0ls7cz5f5n9fwc5lyf2gdnrkx40pwc4jef679xte56jx3jz6mc73t6w7ac53q2qqqnxv8"
