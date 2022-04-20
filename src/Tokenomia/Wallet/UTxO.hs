{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.Wallet.UTxO
    ( UTxO (..)
    ) where

import Tokenomia.Common.Shell.InteractiveMenu
    ( DisplayMenuItem(..) )

import           Prelude as P
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Maybe
import           Data.List as L ( filter, (!!), drop, head )
import           Ledger.Ada          
import           Ledger ( TxOutRef (..) )
import           Control.Monad.Except
import           Data.String ( IsString(fromString) )
import           Tokenomia.Common.Serialise ( FromCLI(..) )
import           Tokenomia.Common.Error
import           Ledger.Value ( Value )
import           Tokenomia.Common.TxOutRef ( showTxOutRef ) 
import           Tokenomia.Wallet.Type () 
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import           Tokenomia.Common.Hash    
import           Tokenomia.Common.Value    
import qualified Data.List.NonEmpty as NEL


data UTxO = UTxO
              { txOutRef :: TxOutRef
              , value :: Value
              , maybeDatumHash :: Maybe Hash } deriving (Show,Eq)

instance Ord UTxO where 
  compare x y = compare (txOutRef x) (txOutRef y)


type ValueTokenAndDatum = Text
type TxOutIndexToken = Text
type TxIdToken = Text


-- 7ab72a8b4fe1128de66e5d95577c0ba213033cbe7153061bf366a5d108c2bb13     0        994933314 lovelace + TxOutDatumHashNone
-- 42e5d56fe31a9ee9bc83b6b88c2254952d9e477ca46e40dc985fe041feec50f2    10        6000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "faf44f2aa43aa67e7a8b7e8c24465515dcf86ed3780c70779e6ac13cd68f3060"

instance FromCLI [UTxO] where
  fromCLI = parse . tokenize
    where
      parse :: [(TxIdToken,TxOutIndexToken,[ValueTokenAndDatum])] -> [UTxO]
      parse = fmap (\(txHash,txIx,valueTokensAndDatum) ->
                      UTxO
                        { txOutRef = TxOutRef((fromString . T.unpack) txHash) ((read @Integer . T.unpack)  txIx)
                        , value = ( fromCLI . T.unwords ) valueTokensAndDatum
                        , maybeDatumHash = (parseDatumMaybe . T.words. P.last . T.splitOn "+". T.unwords) valueTokensAndDatum })


      tokenize :: Text ->  [(TxIdToken,TxOutIndexToken,[ValueTokenAndDatum])]
      tokenize
          =  map (\a ->
                  ( L.head a
                  , a L.!! 1
                  , (filterEmptyLines . L.drop 2) a))
          .  map T.words
          .  (removeHeader . filterEmptyLines . T.lines)

      removeHeader :: [Text] -> [Text]
      removeHeader = L.drop 2

      filterEmptyLines :: [Text] -> [Text]
      filterEmptyLines = L.filter (\a -> T.strip a /= mempty )


parseDatumMaybe :: [Text] -> Maybe Hash
parseDatumMaybe ["TxOutDatumNone"] = Nothing 
parseDatumMaybe ["TxOutDatumHash","ScriptDataInAlonzoEra", hash] = (Just . Hash . T.unpack . T.drop 1 . T.take (T.length hash-1)) hash
parseDatumMaybe x = error $ "unexpected format :" <> show (T.unpack <$> x)