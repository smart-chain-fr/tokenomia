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


module Tokenomia.Common.Shell.InteractiveMenu
    (askSelect) where

import Data.List.NonEmpty (NonEmpty, toList, zip, (!!))
import Prelude hiding ((!!))
import Text.Read (readEither)
import Shh
import Control.Monad.Reader (liftIO)

load SearchPath ["echo", "clear"]

zipIndex :: Show a => Int -> [a] -> [(Int, a)]
zipIndex _ [] = []
zipIndex i (x: xs) = (i, x) : zipIndex (i + 1) xs

echoChoices :: Show a =>  (Int, a) -> Cmd 
echoChoices (i, x) = echo (show i) "-" (show x) 

askSelectRepeatedly :: Show a => NonEmpty a -> IO Int
askSelectRepeatedly choices = do
    let orderedChoices = zipIndex 1 (toList choices)
    mapM_ (liftIO . echoChoices) orderedChoices
    getLine >>= (
        \case
            Left err -> do
                clear
                putStrLn  $ show err ++ ": Wrong parse input. Please try again."
                askSelectRepeatedly choices
            Right ioIdx -> if ioIdx > length choices || ioIdx < 0 then do
                clear
                putStrLn $ show ioIdx ++ ": Number selected is incorrect"
                askSelectRepeatedly choices else return ioIdx) . readEither


askSelect :: Show a => NonEmpty a -> IO a
askSelect choices = (\idx -> choices !! (idx - 1)) <$> askSelectRepeatedly choices