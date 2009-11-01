module SimpleJSON.Test.PrettyJSONTests (tests, main) where

import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck (Property, (==>))
import SimpleJSON
import SimpleJSON.Prettify
import SimpleJSON.PrettyJSON
import SimpleJSON.Test.Utils () -- instance Arbitrary Char

renderJValueTests :: [Test]
renderJValueTests =
    [ testProperty "renderJValue should render a prettified string when provided a JString"

    $ let prop :: String -> Bool
          prop str = string str == renderJValue (JString str)
      in prop

    , testProperty "renderJValue should render a prettified double when provided a JNumber"

    $ let prop :: Double -> Bool
          prop n = double n == renderJValue (JNumber n)
      in prop

    , testProperty "renderJValue should render a prettified 'true' when provided a (JBool True)"

    $ let prop :: Bool -> Property
          prop b = b ==> text "true" == renderJValue (JBool b)
      in prop

    , testProperty "renderJValue should render a prettified 'false' when provided a (JBool False)"

    $ let prop :: Bool -> Property
          prop b = not b ==> text "false" == renderJValue (JBool b)
      in prop

    , testProperty "renderJValue should render a prettified 'null' when provided a JNull"

    $ let prop :: Bool
          prop = text "null" == renderJValue JNull
      in prop

    ]

tests :: [Test]
tests =
    [ testGroup "PrettyJSON"
      [ testGroup "renderJValue" renderJValueTests
      ]
    ]

main :: IO ()
main = defaultMain tests