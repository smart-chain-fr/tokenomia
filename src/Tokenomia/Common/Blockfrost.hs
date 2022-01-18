{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Tokenomia.Common.Blockfrost (projectFromEnv'') where


import qualified Blockfrost.Client as B

import Tokenomia.Common.Environment
    ( Environment(Mainnet, Testnet) )
import Prelude hiding (head)

import Control.Monad.Reader ( MonadReader, MonadIO(..), asks )

projectFromEnv'' 
    :: ( MonadIO m
       , MonadReader Environment m) => m B.Project
projectFromEnv'' = do 
    environmentPath <- asks (\case
                                Testnet {} -> "BLOCKFROST_TOKEN_TESTNET_PATH"
                                Mainnet {} -> "BLOCKFROST_TOKEN_MAINNET_PATH")
    liftIO $ B.projectFromEnv' environmentPath