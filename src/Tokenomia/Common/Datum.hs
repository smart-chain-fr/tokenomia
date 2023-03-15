{-# LANGUAGE ExtendedDefaultRules                      #-}
{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE FlexibleInstances                         #-}
{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE LambdaCase                                #-}
{-# LANGUAGE NamedFieldPuns                            #-}
{-# LANGUAGE RankNTypes                                #-}
{-# LANGUAGE RecordWildCards                           #-}
{-# LANGUAGE TemplateHaskell                           #-}
{-# LANGUAGE TupleSections                             #-}
{-# LANGUAGE TypeApplications                          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures           #-}
{-# OPTIONS_GHC -fno-warn-orphans                      #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds             #-}

module Tokenomia.Common.Datum
    ( getDataHash
    , registerDatum
    ) where

import Data.Aeson
    as Json                                            ( encode )
import Data.ByteString.Lazy.Char8 qualified as C
import Data.ByteString.Lazy.UTF8
    as BLU                                             ( toString )
import Data.Coerce                                     ( coerce )
import Data.String                                     ( IsString(fromString) )

import PlutusTx qualified
import PlutusTx.IsData.Class                           ( ToData )

import Control.Monad.Reader                            ( MonadIO(..), MonadReader )

import Shh.Internal
    ( ExecReference(SearchPath)
    , Stream(Truncate)
    , capture
    , load
    , (&>)
    , (|>)
    )


import Cardano.Api
    ( ScriptDataJsonSchema(ScriptDataJsonDetailedSchema)
    , scriptDataToJson
    )
import Cardano.Api.Shelley                             ( fromPlutusData )

import System.Random                                   ( randomIO )
import Tokenomia.Common.Environment                    ( Environment )
import Tokenomia.Common.Folder                         ( Folder(..), getFolderPath )
import Tokenomia.Common.Hash                           ( Hash(..) )

load SearchPath ["cat","cardano-cli", "echo" ]

dataToJSONString :: ToData a => a -> String
dataToJSONString =
  BLU.toString
    . Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData

getDataHash :: (MonadIO m, MonadReader Environment m, ToData a) => a -> m Hash
getDataHash a = do
    tmpFolder <- getFolderPath TMP
    randomInt <- liftIO ( abs <$> randomIO :: IO Integer)
    let filePath = tmpFolder <> "datum-" <> show randomInt <> ".json"
    liftIO $ echo  (dataToJSONString a) &> (Truncate . fromString) filePath
    liftIO $ Hash . init . C.unpack  <$>  (cardano_cli "transaction" "hash-script-data" "--script-data-file" filePath |> capture)

registerDatum
  :: ( MonadIO m
     , MonadReader Environment m
     , ToData a) => a -> m FilePath
registerDatum a = do
    datumFolder <- getFolderPath Datum
    datumHash <- getDataHash a
    let filePath = datumFolder <> coerce datumHash <> ".datum"
    liftIO $ echo  (dataToJSONString a) &> (Truncate . fromString) filePath
    return filePath
