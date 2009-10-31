module SimpleJSON.Test.JValueTests (tests) where

import Data.Char
import Control.Monad (liftM)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck
import SimpleJSON

getStringTests :: [Test]
getStringTests = 
    [ testProperty "getString should return Just the string value when given a JString"

    $ let prop :: JValue -> Bool
          prop jvalue =
              case jvalue of
                JString s -> getString jvalue == Just s
                _         -> True
      in prop


    , testProperty "getString should return Nothing when not given a JString"

    $ let prop :: JValue -> Bool
          prop jvalue =
              case jvalue of
                JString _ -> True
                _         -> getString jvalue == Nothing
      in prop

    ]

getIntTests :: [Test]
getIntTests =
    [ testProperty "getInt should return Just the int value when given a JNumber"

    $ let prop :: JValue -> Bool
          prop jvalue =
              case jvalue of
                JNumber n -> getInt jvalue == Just (truncate n)
                _         -> True
      in prop


    , testProperty "getInt should return Nothing when not given a JNumber"

    $ let prop :: JValue -> Bool
          prop jvalue =
              case jvalue of
                JNumber _ -> True
                _         -> getInt jvalue == Nothing
      in prop

    ]

getDoubleTests :: [Test]
getDoubleTests =
    [ testProperty "getDouble should return Just the double value when given a JNumber"

    $ let prop :: JValue -> Bool
          prop jvalue =
              case jvalue of
                JNumber n -> getDouble jvalue == Just n
                _         -> True
      in prop


    , testProperty "getDouble should return Nothing when not given a JNumber"

    $ let prop :: JValue -> Bool
          prop jvalue =
              case jvalue of
                JNumber _ -> True
                _         -> getDouble jvalue == Nothing
      in prop

    ]

getBoolTests :: [Test]
getBoolTests =
    [ testProperty "getBool should return Just the bool value when given a JBool"

    $ let prop :: JValue -> Bool
          prop jvalue =
              case jvalue of
                JBool b -> getBool jvalue == Just b
                _       -> True
      in prop


    , testProperty "getBool should return Nothing when not given a JBool"

    $ let prop :: JValue -> Bool
          prop jvalue =
              case jvalue of
                JBool _ -> True
                _       -> getBool jvalue == Nothing
      in prop

    ]

getObjectTests :: [Test]
getObjectTests = 
    [ testProperty "getObject should return Just the object value when given a JObject"

    $ let prop :: JValue -> Bool
          prop jvalue =
              case jvalue of
                JObject object -> getObject jvalue == Just object
                _              -> True
      in prop


    , testProperty "getObject should return Nothing when not given a JObject"

    $ let prop :: JValue -> Bool
          prop jvalue =
              case jvalue of
                JObject _ -> True
                _         -> getObject jvalue == Nothing
      in prop

    ]

getArrayTests :: [Test]
getArrayTests =
    [ testProperty "getArray should return Just the array value when given a JArray"

    $ let prop :: JValue -> Bool
          prop jvalue =
              case jvalue of
                JArray array -> getArray jvalue == Just array
                _            -> True
      in prop

    , testProperty "getArray should return Nothing when not given a JArray"

    $ let prop :: JValue -> Bool
          prop jvalue =
              case jvalue of
                JArray _ -> True
                _        -> getArray jvalue == Nothing
      in prop

    ]


tests :: [Test]
tests = [ testGroup "JValue"
          [ testGroup "getString" getStringTests
          , testGroup "getInt"    getIntTests
          , testGroup "getDouble" getDoubleTests
          , testGroup "getBool"   getBoolTests
          , testGroup "getDouble" getDoubleTests
          , testGroup "getObject" getObjectTests
          , testGroup "getArray"  getArrayTests
          ]
        ]


instance Arbitrary JValue where
    arbitrary   = arbitraryJValue
    coarbitrary = undefined


arbitraryJValue :: Gen JValue
arbitraryJValue = sized arbitraryJValue'

arbitraryJValue' :: Int -> Gen JValue
arbitraryJValue' n | n > 0 = oneof (nonRecursiveJValues ++ recursiveJValues n)
arbitraryJValue' _         = oneof nonRecursiveJValues

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


