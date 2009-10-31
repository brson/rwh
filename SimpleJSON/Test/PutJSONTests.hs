module SimpleJSON.Test.PutJSONTests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)
import SimpleJSON
import SimpleJSON.PutJSON
import SimpleJSON.Test.Utils ()

renderJValueTests :: [Test]
renderJValueTests =
    [ testProperty "renderJValue should render a JString the same as showing the contained string"

    $ let prop :: String -> Bool
          prop s = renderJValue (JString s) == show s
      in prop

    ]

tests :: [Test]
tests = 
    [ testGroup "PutJSON"
      [ testGroup "renderJValue" renderJValueTests
      ]
    ]
