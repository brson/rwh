module SimpleJSON.Test.PrettifyTests (tests, main) where

import Control.Monad (liftM)
import qualified Control.OldException as E
import Data.Char (ord, chr)
import Numeric (showHex)
import Test.Framework as TestFramework (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import Test.HUnit
import SimpleJSON.Prettify
import SimpleJSON.Test.Utils () -- instance Arbitrary Char

prettifyTests :: [TestFramework.Test]
prettifyTests =
    [ testCase "empty should return the Empty Doc"

    $ let prop :: Assertion
          prop = Empty @=? empty
      in prop

    , testProperty "char should return the Char Doc"

    $ let prop :: Char -> Bool
          prop c = Char c == char c
      in prop

    , testProperty "text should return the Text Doc if the provided String is not empty"

    $ let prop :: String -> Property
          prop str = (str /= "") ==> Text str == text str
      in prop

    , testCase "text should return the Empty Doc if the provided String is empty"

    $ let prop :: Assertion
          prop = Empty @=? text ""
      in prop

    , testProperty "double should return the Text Doc containing the serialized number"

    $ let prop :: Double -> Bool
          prop n = Text (show n) == double n
      in prop

    , testGroup "<>" concatTests

    , testGroup "string" stringTests

    , testGroup "oneChar" oneCharTests

    , testGroup "smallHex" smallHexTests

    ]

concatTests :: [TestFramework.Test]
concatTests =
    [ testProperty "a <> b should return the non-empty Doc when one of the given Docs is Empty"

    $ let prop :: Doc -> Doc -> Property
          prop doc1 doc2 =
              (doc1 == Empty) /= (doc2 == Empty)
              ==> doc1 <> doc2 == if doc1 /= Empty then doc1 else doc2
      in prop

    , testCase "a <> b should return the Empty Doc when both of the given Docs are Empty"

    $ let prop :: Assertion
          prop = Empty @=? Empty <> Empty
      in prop

    , testProperty "a <> b should return a Concat b"

    $ let prop :: Doc -> Doc -> Property
          prop doc1 doc2 =
              doc1 /= Empty && doc2 /= Empty
              ==> doc1 <> doc2 == doc1 `Concat` doc2
      in prop

    ]

stringTests :: [TestFramework.Test]
stringTests =
    [ testProperty "string should enclose the string in quotes"

    $ let prop :: String -> Bool
          prop str = 
              let expected = char '"' <> escapeString str <> char '"'
              in expected == string str
      in prop

    ]

oneCharTests :: [TestFramework.Test]
oneCharTests =
    [ testProperty "oneChar should wrap a Char in a Doc"

    $ let prop :: Char -> Property
          prop c = not (isSimpleEscapeChar c) ==> oneChar c == char c
      in prop

    , testProperty "oneChar should escape standard unprintable characters"

    $ let prop :: Property
          prop = forAll (elements simpleEscapes) $ verifyProperty
          verifyProperty :: (Char, String) -> Bool
          verifyProperty (unescapedChar, escapedString) = oneChar unescapedChar == text escapedString
          simpleEscapes :: [(Char, String)]
          simpleEscapes = zipWith escapePair simpleEscapeChars simpleEscapeReplacements
          escapePair :: Char -> Char -> (Char, String)
          escapePair unescapedChar escapeReplacement = (unescapedChar, ['\\', escapeReplacement])
      in prop

    , testProperty "oneChar should escape characters less than ASCII 32 (space) unless they are standard unprintable characters"

    $ let prop :: Property
          prop = forAll (elements charsLessThanSpace) $ verifyProperty
          verifyProperty :: Char -> Property
          verifyProperty ch = not (isSimpleEscapeChar ch) ==> oneChar ch == smallHex (ord ch)
          charsLessThanSpace :: [Char]
          charsLessThanSpace = [chr 0 .. chr 31]
      in prop

    ]

smallHexTests :: [TestFramework.Test]
smallHexTests =
    [ testProperty "smallHex should prefix it's result with the Text Doc \"\\u\""

    $ let prop :: Bool
          prop = undefined
      in prop

    , testProperty "smallHex should pad the hex value with up to 4 0's"

    $ let prop :: Bool
          prop = undefined
      in prop

    , testProperty "smallHex should display the hex value of an integer"

    $ let prop :: Int -> Property
          prop i = i >= 0 ==> smallHex i == text (showHex i "")
      in prop

    , testCase "smallHex should fail if the provided value is less than 0"

    $ let test :: Assertion
          test = E.catch performCall handleException
          performCall :: Assertion
          performCall =
              do E.evaluate (smallHex (-1))
                 assertFailure "smallHex should throw an error"
          handleException :: E.Exception -> IO ()
          handleException e = return ()
      in test

    ]

tests :: [TestFramework.Test]
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

simpleEscapeChars :: [Char]
simpleEscapeChars = "\b\n\f\r\t\\\"/"

simpleEscapeReplacements :: [Char]
simpleEscapeReplacements = "bnfrt\\\"/"

isSimpleEscapeChar :: Char -> Bool
isSimpleEscapeChar c = elem c simpleEscapeChars
