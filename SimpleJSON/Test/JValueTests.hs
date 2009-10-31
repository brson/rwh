module SimpleJSON.Test.JValueTests (tests) where

import Data.Char
import Control.Monad (liftM)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck
import SimpleJSON

jValueTestGroup :: Test
jValueTestGroup = 
    testGroup "JValue"
                  [ testProperty "getString should return Just the string value when given a JString"

                  $ let prop jvalue@(JString s) = getString jvalue == Just s
                        prop jvalue             = getString jvalue == Nothing
                    in prop

                  ]

instance Arbitrary JValue where
    arbitrary   = arbitraryJValue
    coarbitrary = undefined


arbitraryJValue :: Gen JValue
arbitraryJValue = sized arbitraryJValue'

arbitraryJValue' :: Int -> Gen JValue
arbitraryJValue' 0 = oneof nonRecursiveJValues
arbitraryJValue' n | n > 0 =
                       oneof (nonRecursiveJValues ++ recursiveJValues n)

nonRecursiveJValues :: [Gen JValue]
nonRecursiveJValues =
    [ liftM JString arbitrary
    , liftM JNumber arbitrary
    , liftM JBool arbitrary
    , elements [JNull]
    ]

recursiveJValues :: Int -> [Gen JValue]
recursiveJValues n =
    [ liftM JObject subValue
    , liftM JArray subValue
    ]
    where subValue :: Arbitrary a => Gen a
          subValue = resize (n `div` 2) arbitrary

instance Arbitrary Char where
    arbitrary     = choose (32, 255) >>= \n -> return (chr n)
    coarbitrary n = variant (ord n)


tests :: [Test]
tests = [jValueTestGroup]