module SimpleJSON.Test.PutJSONTests (tests, main) where

import Data.List (intercalate)
import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck
import SimpleJSON
import SimpleJSON.PutJSON
import SimpleJSON.Test.JValueTests () -- instance Arbitrary JValue
import SimpleJSON.Test.Utils () -- instance Arbitrary Char

renderJValueTests :: [Test]
renderJValueTests =
    [ testProperty "renderJValue should render a JString the same as showing the contained string"

    $ let prop :: String -> Bool
          prop s = renderJValue (JString s) == show s
      in prop

    , testProperty "renderJValue should render a JNumber the same as showing the contained number"

    $ let prop :: Double -> Bool
          prop n = renderJValue (JNumber n) == show n
      in prop

    , testProperty "renderJValue should render a JBool containing True as 'true'"

    $ let prop :: Bool -> Property
          prop b = b ==> renderJValue (JBool b) == "true"
      in prop

    , testProperty "renderJValue should render a JBool containing False as 'false'"

    $ let prop :: Bool -> Property
          prop b = not b ==> renderJValue (JBool b) == "false"
      in prop

    , testProperty "renderJValue should render a JNull as 'null'"

    $ let prop :: Bool
          prop = renderJValue JNull == "null"
      in prop

    , testGroup "renderJValue should render a JObject's members"

      renderJValueOfJObjectTests

    , testProperty "renderJValue should render a JArray's elements"

    -- This property just duplicates the implementation...
    $ let prop :: [JValue] -> Bool
          prop elems = 
              let expected = "[" ++ values elems ++ "]"
                  values [] = ""
                  values vs = intercalate ", " (map renderJValue vs)
              in expected == renderJValue (JArray elems)
      in prop

    ]

renderJValueOfJObjectTests :: [Test]
renderJValueOfJObjectTests =
    [ testProperty "vs model"

    $ let prop :: [(String, JValue)] -> Bool
          prop members = 
              let expected = "{" ++ pairs members ++ "}"
                  pairs [] = ""
                  pairs ps = intercalate ", " (map renderPair ps)
                  renderPair (k, v) = show k ++ ": " ++ renderJValue v
              in expected == renderJValue (JObject members)
      in prop

    , testProperty "by example"

    $ let prop :: Property
          prop = forAll examples compareResults
          compareResults :: ([(String, JValue)], String) -> Bool
          compareResults (pairs, expected) = renderJValue (JObject pairs) == expected
          examples :: Gen ([(String, JValue)], String)
          examples = elements [ ([("test", JString "test")], "{\"test\": \"test\"}")
                              , ([("test", JNumber 1)], "{\"test\": 1.0}")
                              , ([("test", JBool True)], "{\"test\": true}")
                              , ([], "{}")
                              ]
      in prop

    , testProperty "by example 2"

    $ let prop :: String -> JValue -> Property
          prop string jvalue = forAll (examples string jvalue) compareResults
          compareResults :: ([(String, JValue)], String) -> Bool
          compareResults (pairs, expected) = renderJValue (JObject pairs) == expected
          examples :: String -> JValue -> Gen ([(String, JValue)], String)
          examples string jvalue =
              elements [ ([(string, jvalue)]
                         , "{" ++ (show string) ++ ": " ++ (renderJValue jvalue) ++ "}")
                       ]
      in prop

    ]



tests :: [Test]
tests = 
    [ testGroup "PutJSON"
      [ testGroup "renderJValue" renderJValueTests
      ]
    ]

main :: IO ()
main = defaultMain tests