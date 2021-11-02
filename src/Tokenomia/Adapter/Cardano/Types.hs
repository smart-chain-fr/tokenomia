{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tokenomia.Adapter.Cardano.Types (Address (..), Hash(..)) where

newtype Address = Address String deriving stock (Eq,Show) 
                                 deriving newtype Read 

newtype Hash = Hash String deriving (Show,Eq)