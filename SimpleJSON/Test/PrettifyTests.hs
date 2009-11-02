module SimpleJSON.Test.PrettifyTests (tests, main) where

import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck (Property, (==>))
import SimpleJSON.Prettify
import SimpleJSON.Test.Utils () -- instance Arbitrary Char

prettifyTests :: [Test]
prettifyTests =
    [ testProperty "empty should return the Empty Doc"

    $ let prop :: Bool
          prop = Empty == empty
      in prop

    , testProperty "char should return the Char Doc"

    $ let prop :: Char -> Bool
          prop c = Char c == char c
      in prop

    , testProperty "text should return the Text Doc if the provided String is not empty"

    $ let prop :: String -> Property
          prop str = (str /= "") ==> Text str == text str
      in prop

    , testProperty "text should return the Empty Doc if the provided String is empty"

    $ let prop :: String -> Property
          prop str = (str == "") ==> Empty == text str
      in prop

    , testProperty "double should return the Text Doc containing the serialized number"

    $ let prop :: Double -> Bool
          prop n = Text (show n) == double n
      in prop
    ]

tests :: [Test]
tests = 
    [ testGroup "Prettify"
      prettifyTests
    ]

main :: IO ()
main = defaultMain tests