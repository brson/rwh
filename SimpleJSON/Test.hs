module SimpleJSON.Test (tests, main) where

import Test.Framework (Test, defaultMain)
import qualified SimpleJSON.Test.JValueTests as JValueTests
import qualified SimpleJSON.Test.PutJSONTests as PutJSONTests

tests :: [Test]
tests = JValueTests.tests ++ PutJSONTests.tests

main :: IO ()
main = defaultMain tests