module SimpleJSON.Test.JValueTests (tests, main) where

import Control.Monad (liftM)
import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck
import SimpleJSON
import SimpleJSON.Test.Utils ()

getStringTests :: [Test]
getStringTests = 
    [ testProperty "getString should return Just the string value when given a JString"

    $ let prop :: String -> Bool
          prop s = getString (JString s) == Just s
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

    $ let prop :: Int -> Bool
          prop n = getInt (JNumber (fromIntegral n)) == Just n
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

    $ let prop :: Double -> Bool
          prop n = getDouble (JNumber n) == Just n
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

    $ let prop :: Bool -> Bool
          prop b = getBool (JBool b) == Just b
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

    $ let prop :: [(String, JValue)] -> Bool
          prop o = getObject (JObject o) == Just o
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

    $ let prop :: [JValue] -> Bool
          prop a = getArray (JArray a) == Just a
      in prop

    , testProperty "getArray should return Nothing when not given a JArray"

    $ let prop :: JValue -> Bool
          prop jvalue =
              case jvalue of
                JArray _ -> True
                _        -> getArray jvalue == Nothing
      in prop

    ]

isNullTests :: [Test]
isNullTests =
    [ testProperty "isNull should return True when given a JNull"
    -- TODO: This only needs to be checked once, not 100 times
    $ let prop :: Bool
          prop = isNull (JNull)
      in prop

    , testProperty "isNull should return False when not given a JNull"

    $ let prop :: JValue -> Bool
          prop jvalue =
              case jvalue of
                JNull -> True
                _     -> isNull jvalue == False
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
          , testGroup "isNull"    isNullTests
          ]
        ]

main :: IO ()
main = defaultMain tests

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



