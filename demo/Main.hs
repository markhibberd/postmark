module Main where

import Network.Postmark.Demo

main ::
  IO ()
main =
  demo >>= print
