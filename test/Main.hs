module Main where

import qualified Network.Api.Postmark.Tests
import Test.Framework

main ::
  IO ()
main =
  defaultMain tests

tests ::
  [Test]
tests =
  [
    testGroup "Tests"
      [
        Postmark.Tests.test
      ]
  ]
