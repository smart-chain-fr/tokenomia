{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Tokenomia.ICO.Funds.WhiteListing.Types
    ( Payload (..)
    , Investor (..)
    ) where

import Prelude hiding (round,print)

import Data.Aeson


data Payload 
    = Payload 
      {status :: String
      ,message :: String
      ,investors :: [Investor] } deriving Show

instance FromJSON Payload where
    parseJSON = withObject "Payload" $ \v -> Payload
        <$> v .: "STATUS"
        <*> v .: "MESSAGE"
        <*> v .: "DATA"

data Investor = Investor 
                { index :: Integer
                , paybackAddress :: String 
                , childAddress :: String } deriving (Eq,Show)
                
instance Ord Investor where
    compare x y = compare (index x ) (index y)

instance FromJSON Investor where
    parseJSON = withObject "Investor" $ \v -> Investor
        <$> v .: "id"
        <*> v .: "participation_address"
        <*> v .: "reception_address"
        
