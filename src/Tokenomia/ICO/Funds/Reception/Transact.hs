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
import           Tokenomia.Common.Environment
import           Tokenomia.Common.Transacting

import           Tokenomia.ICO.Funds.Reception.Command
import           Tokenomia.Common.Error
import           Tokenomia.ICO.Funds.Reception.Plan

transact 
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError TokenomiaError m)
    => AddressFundsPlan  -> m ()   
transact  _  
    = return () -- Todo
