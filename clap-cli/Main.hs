{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where 

import Shh

load SearchPath ["echo","ls","sha256sum","cat"]

main :: IO ()
main = do 
    echo "Hello Charles A with Echo" 
    ls "-al"
    "Hello" >>> sha256sum 