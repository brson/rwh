module SimpleJSON.Test (tests, main) where

import Test.Framework (Test, defaultMain)
import qualified SimpleJSON.Test.JValueTests as JValueTests
import qualified SimpleJSON.Test.PutJSONTests as PutJSONTests
import qualified SimpleJSON.Test.PrettyJSONTests as PrettyJSONTests

tests :: [Test]
tests =
    JValueTests.tests ++
    PutJSONTests.tests ++
    PrettyJSONTests.tests

main :: IO ()
main = defaultMain tests