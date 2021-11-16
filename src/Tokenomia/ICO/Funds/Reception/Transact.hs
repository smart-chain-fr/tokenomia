{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
module Tokenomia.ICO.Funds.Reception.Transact
    (transact) where

import           Prelude hiding (round,print)
import           Control.Monad.Reader hiding (ask)
import           Control.Monad.Except
import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Adapter.Cardano.CLI.Transaction

import           Tokenomia.ICO.Funds.Reception.Types


transact 
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)
    => AddressFundsPlan  -> m ()   
transact  _  
    = return () -- Todo
