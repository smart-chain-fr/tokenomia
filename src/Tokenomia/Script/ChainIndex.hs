{-# LANGUAGE ExtendedDefaultRules                      #-}
{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE FlexibleInstances                         #-}
{-# LANGUAGE LambdaCase                                #-}
{-# LANGUAGE NamedFieldPuns                            #-}
{-# LANGUAGE RankNTypes                                #-}
{-# LANGUAGE RecordWildCards                           #-}
{-# LANGUAGE TemplateHaskell                           #-}
{-# LANGUAGE TupleSections                             #-}
{-# LANGUAGE TypeApplications                          #-}


{-# LANGUAGE ImportQualifiedPost                       #-}
{-# OPTIONS_GHC -Wno-orphans                           #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures           #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds             #-}

module Tokenomia.Script.ChainIndex
    ( queryUTxO
    ) where


import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding
    as TLE                                             ( decodeUtf8 )

import Control.Monad.Reader                            ( MonadIO(..), MonadReader, asks )
import Shh.Internal                                    ( ExecReference(SearchPath), capture, load, (|>) )


import Tokenomia.Common.Address                        ( Address(..) )
import Tokenomia.Common.Environment                    ( Environment(magicNumber) )
import Tokenomia.Common.Serialise                      ( FromCLI(fromCLI) )
import Tokenomia.Common.Value                          ()
import Tokenomia.Script.UTxO                           ( ScriptUTxO )


load SearchPath ["cardano-cli"]

queryUTxO ::
  ( MonadIO m
  , MonadReader Environment m )
  => Address
  -> m [ScriptUTxO]
queryUTxO (Address address) = do
    magicN <- asks magicNumber
    fromCLI . TL.toStrict . TLE.decodeUtf8 <$> liftIO (cardano_cli "query" "utxo" "--testnet-magic" magicN "--address" address |> capture)
