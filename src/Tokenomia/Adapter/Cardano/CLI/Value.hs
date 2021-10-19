{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tokenomia.Adapter.Cardano.CLI.Value () where

import Tokenomia.Adapter.Cardano.CLI.Serialise 

import qualified Data.Text as T
import Data.Text (Text)

import Data.List as L

import Plutus.V1.Ledger.Value

import Data.String


instance FromCLI Value  where
   fromCLI = parse . tokenize
     where 
      parse ::  [(Text,Text,Text)] -> Value
      parse =
        foldMap (
          ( \(a,b,c) -> singleton a b c) 
          .(\(a,b,c) ->
              ( (fromString . T.unpack )    a
              , (fromString . T.unpack)     b
              , (read @Integer . T.unpack)  c )))

      tokenize :: Text ->  [(Text,Text,Text)]
      tokenize 
        = tokenizeToken
          . removeDatum
          . map T.words
          . T.splitOn "+" 
        where
          tokenizeToken :: [[Text]] -> [(Text,Text,Text)]
          tokenizeToken 
            = fmap (\case
                [a,"lovelace"] -> ("","", a)
                [a,b] -> case T.splitOn "." b of
                          [ph,tn] -> (ph,tn,a)
                          x -> error $ "unexpected format :" <> show (T.unpack <$> x)
                x ->  error $ "unexpected format :" <> show (T.unpack <$> x) )
          removeDatum = L.filter (\a -> 2 == L.length a)