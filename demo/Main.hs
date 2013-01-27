module Main where

import Network.Api.Postmark.Demo

main ::
  IO ()
main =
  demo >>= print
