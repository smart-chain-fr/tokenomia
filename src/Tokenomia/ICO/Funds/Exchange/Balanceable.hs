{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}

module Tokenomia.ICO.Funds.Exchange.Balanceable
    ( AdaBalanceable (..)
    , TokenBalanceable (..) ) where
import           Prelude hiding (round,print)
import           Data.Set.NonEmpty as NES
import Data.Monoid
import Data.Coerce
import Ledger.Ada


class AdaBalanceable a where 
    adaBalance   :: a -> Ada

class TokenBalanceable a where    
    tokenBalance :: a -> Integer

instance AdaBalanceable a => AdaBalanceable (NESet a) where
    adaBalance xs = foldMap adaBalance (toList xs)
instance TokenBalanceable a => TokenBalanceable (NESet a) where
    tokenBalance xs = coerce $ foldMap (Sum . tokenBalance) (toList xs)  


instance AdaBalanceable Ada where
    adaBalance ada = ada 

instance AdaBalanceable a => AdaBalanceable (Maybe a) where 
    adaBalance Nothing = 0
    adaBalance (Just a) = adaBalance a

instance TokenBalanceable a => TokenBalanceable (Maybe a) where 
    tokenBalance Nothing = 0
    tokenBalance (Just a) = tokenBalance a 
