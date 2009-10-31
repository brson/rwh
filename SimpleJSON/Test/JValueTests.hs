module SimpleJSON.Test.JValueTests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)

jValueTestGroup :: Test
jValueTestGroup = 
    testGroup "JValue"
                  [ testProperty "test" True ]


tests :: [Test]
tests = [jValueTestGroup]