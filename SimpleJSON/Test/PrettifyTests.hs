module SimpleJSON.Test.PrettifyTests (tests, main) where

import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck (testProperty)
import SimpleJSON.Prettify
import SimpleJSON.Test.Utils () -- instance Arbitrary Char

tests :: [Test]
tests = 
    [ testGroup "Prettify"
      []
    ]

main :: IO ()
main = defaultMain tests