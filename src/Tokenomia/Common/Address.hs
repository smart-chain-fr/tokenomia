{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tokenomia.Common.Address 
    ( Address (..)) where

import Data.String (IsString)
newtype Address = Address String deriving stock (Eq,Ord) 
                                 deriving (Show,IsString) via String 
                                 
