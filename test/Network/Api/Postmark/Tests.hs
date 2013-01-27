module Network.Api.Postmark.Tests
  (
    main
  , test
  ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Network.Api.Postmark

main ::
  IO ()
main =
  defaultMain [test]

test ::
  Test
test =
    testGroup "Postmark"
      [
        testProperty "Identity" prop_identity
      ]

prop_identity ::
  Int
  -> Bool
prop_identity n =
  n == n
