module Main where

import Postmark.Demo

main ::
  IO ()
main =
  demo >>= print


