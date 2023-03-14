{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Tokenomia.Common.Shell.Console
    ( printLn
    , print
    , printOpt
    , clearConsole) where

import Shh (load, ExecReference(SearchPath))
import Control.Monad.Reader (MonadIO, liftIO)
import Prelude hiding (print)

load SearchPath ["echo", "clear"]

printLn :: MonadIO m => String -> m ()
printLn string = liftIO $ echo string

print :: MonadIO m => String -> m ()
print string = liftIO $ echo "-n" string

printOpt :: MonadIO m => String -> String -> m ()
printOpt opt string = liftIO $ echo opt string

clearConsole :: MonadIO m => m ()
clearConsole = liftIO clear
