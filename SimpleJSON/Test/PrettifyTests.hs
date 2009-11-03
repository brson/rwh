module SimpleJSON.Test.PrettifyTests (tests, main) where

import Control.Monad (liftM)
import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck
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

    , testGroup "<>" concatTests

    ]

concatTests :: [Test]
concatTests =
    [ testProperty "a <> b should return the non-empty Doc when one of the given Docs is Empty"

    $ let prop :: Doc -> Doc -> Bool
          prop Empty Empty = True -- don't care
          prop Empty doc   = Empty <> doc == doc
          prop doc   Empty = doc <> empty == doc
          prop _     _     = True -- don't care
      in prop

    , testProperty "a <> b should return the Empty Doc when both of the given Docs are Empty"

    $ let prop :: Doc -> Doc -> Bool
          prop Empty Empty = Empty <> Empty == Empty
          prop _     _     = True -- don't care
      in prop

    , testProperty "a <> b should return a Concat b"

    $ let prop :: Doc -> Doc -> Bool
          prop Empty _     = True -- don't care
          prop _     Empty = True -- don't care
          prop doc1  doc2  = doc1 <> doc2 == doc1 `Concat` doc2
      in prop

    ]

tests :: [Test]
tests = 
    [ testGroup "Prettify"
      prettifyTests
    ]

main :: IO ()
main = defaultMain tests


instance Arbitrary Doc where
    arbitrary   = arbitraryDoc
    coarbitrary = undefined

arbitraryDoc :: Gen Doc
arbitraryDoc = 
    oneof [ elements [Empty]
          , liftM Char arbitrary
          , liftM Text arbitrary
          , elements [Line]
          ] -- TODO Concat & Union
