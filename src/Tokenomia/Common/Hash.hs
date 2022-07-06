module Tokenomia.Common.Hash (Hash (..)) where

newtype Hash = Hash String deriving stock (Show, Eq, Ord)
