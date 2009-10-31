module SimpleJSON.Test (tests) where

import Test.Framework (Test, defaultMain)
import qualified SimpleJSON.Test.JValueTests as JValueTests

tests :: [Test]
tests = JValueTests.tests

main :: IO ()
main = defaultMain tests