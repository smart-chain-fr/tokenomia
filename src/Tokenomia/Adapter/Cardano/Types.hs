{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tokenomia.Adapter.Cardano.Types (Address (..), Hash(..)) where

import Data.String (IsString)
newtype Address = Address String deriving stock (Eq,Ord) 
                                 deriving newtype Show 
                                 deriving newtype IsString

newtype Hash = Hash String deriving (Show,Eq)