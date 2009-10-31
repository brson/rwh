module SimpleJSON.Test.PutJSONTests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck
import SimpleJSON
import SimpleJSON.PutJSON
import SimpleJSON.Test.Utils ()

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

    ]

tests :: [Test]
tests = 
    [ testGroup "PutJSON"
      [ testGroup "renderJValue" renderJValueTests
      ]
    ]
