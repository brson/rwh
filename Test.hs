import Test.Framework (Test, defaultMain)
import qualified SimpleJSON.Test as SimpleJSONTests

tests :: [Test]
tests = SimpleJSONTests.tests

main :: IO ()
main = defaultMain tests