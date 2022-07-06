module Tokenomia.ICO.Funds.WhiteListing.Types (
  Payload (..),
  Investor (..),
) where

import Prelude hiding (print, round)

import Data.Aeson

data Payload = Payload
  { status :: String
  , message :: String
  , investors :: [Investor]
  }
  deriving stock (Show)

instance FromJSON Payload where
  parseJSON = withObject "Payload" $ \v ->
    Payload
      <$> v .: "STATUS"
      <*> v .: "MESSAGE"
      <*> v .: "DATA"

data Investor = Investor
  { index :: Integer
  , paybackAddress :: String
  , childAddress :: String
  }
  deriving stock (Eq, Show)

instance Ord Investor where
  compare x y = compare (index x) (index y)

instance FromJSON Investor where
  parseJSON = withObject "Investor" $ \v ->
    Investor
      <$> v .: "id"
      <*> v .: "reception_address"
      <*> v .: "participation_address"
