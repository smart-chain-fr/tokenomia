{-# LANGUAGE DerivingVia #-}
module Tokenomia.Common.Address
    ( Address (..)) where

import      Data.String (IsString)
import      Data.Aeson  (FromJSON, ToJSON)

newtype Address = Address String deriving stock (Eq,Ord)
                                 deriving (Show,IsString) via String
                                 deriving (ToJSON,FromJSON) via String
