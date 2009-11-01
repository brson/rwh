module SimpleJSON.Test.PrettyJSONTests (tests, main) where

import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck (testProperty)
import SimpleJSON
import SimpleJSON.Prettify
import SimpleJSON.PrettyJSON

tests :: [Test]
tests =
    [ testGroup "PrettyJSON"
      [ ]
    ]

main :: IO ()
main = defaultMain tests