{-# LANGUAGE OverloadedStrings #-}

import System.Environment

import Network.Api.Postmark.Demo

main :: IO ()
main = do
  args <- getArgs
  dispatch args >>= print
