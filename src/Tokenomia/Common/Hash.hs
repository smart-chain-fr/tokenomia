{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tokenomia.Common.Hash 
    ( Hash(..)) where

newtype Hash = Hash String deriving (Show,Eq,Ord)