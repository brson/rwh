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

    , testProperty "surrogatePair should calculate the surrogate pair for an astral character"

    $ let prop :: Property
          prop = forAll (elements astralChars) $ verifyProperty
          verifyProperty :: Char -> Bool
          verifyProperty ch = surrogatePair ch == expected ch
          expected :: Char -> (Int, Int)
          expected c = (h (ord c), l (ord c))
          h :: Int -> Int
          h c = (c - 0x10000) `div` 0x400 + 0xd800
          l :: Int -> Int
          l c = (c - 0x10000) `mod` 0x400 + 0xdc00
      in prop

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
          prop c = not (isSimpleEscapeCharOrDel c) ==> oneChar c == char c
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
          prop = forAll (elements charsLessThanSpace) $ oneCharSmallHexProperty
          charsLessThanSpace :: [Char]
          charsLessThanSpace = [chr 0 .. chr 31]
      in prop

    , testProperty "oneChar should escape the delete character (0x7f)"

    $ let prop :: Property
          prop = forAll (elements ['\x7f']) $ oneCharSmallHexProperty
      in prop

    , testProperty "oneChar should escape characters greater than 0xff"

    $ let prop :: Property
          prop = forAll (elements charsGreaterThanHexFF) $ oneCharSmallHexProperty
          charsGreaterThanHexFF :: [Char]
          charsGreaterThanHexFF = [chr 0x100 .. chr 0xffff] -- chars greater than \xffff are handled by another rule
      in prop

    , testProperty "oneChar should use surrogate pairs for astral unicode characters"

    $ let prop :: Property
          prop = forAll (elements astralChars) $ verifyProperty
          verifyProperty :: Char -> Bool
          verifyProperty ch =
              let (ord1, ord2) = surrogatePair ch
              in oneChar ch == smallHex ord1 <> smallHex ord2
      in prop
    ]

oneCharSmallHexProperty :: Char -> Property
oneCharSmallHexProperty ch = not (isSimpleEscapeChar ch) ==> oneChar ch == smallHex (ord ch)

smallHexTests :: [TestFramework.Test]
smallHexTests =
    [ testProperty "smallHex should prefix it's result with the Text Doc \"\\u\""

    $ let prop :: Int -> Property
          prop i = i >= 0 ==> firstDoc (smallHex i) == expectedEscapeDoc i
          expectedEscapeDoc :: Int -> Doc
          expectedEscapeDoc i = text "\\u"
          firstDoc :: Doc -> Doc
          firstDoc ((a `Concat` b) `Concat` c) = a
      in prop

    , testProperty "smallHex should pad the hex value with up to 4 0's"

    $ let prop :: Int -> Property
          prop i = i >= 0 ==> secondDoc (smallHex i) == expectedPaddingDoc i
          expectedPaddingDoc :: Int -> Doc
          expectedPaddingDoc i = text (replicate (zeroLength i) '0')
          zeroLength :: Int -> Int
          zeroLength i = 4 - length (shortHex i)
          shortHex :: Int -> String
          shortHex h = showHex h ""
          secondDoc :: Doc -> Doc
          secondDoc ((a `Concat` b) `Concat` c) = b
      in prop

    , testProperty "smallHex should display the hex value of an integer"

    $ let prop :: Int -> Property
          prop i = i >= 0 ==> thirdDoc (smallHex i) == text (showHex i "")
          thirdDoc :: Doc -> Doc
          thirdDoc (a `Concat` b) = b
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

isSimpleEscapeCharOrDel :: Char -> Bool
isSimpleEscapeCharOrDel c = (elem c simpleEscapeChars) || c == '\DEL'

astralChars :: [Char]
astralChars = [chr 0x10000 .. chr 0x10ffff]
