module SimpleJSON.Test.Utils where

import Data.Char
import Test.QuickCheck

instance Arbitrary Char where
    arbitrary     = choose (32, 255) >>= \n -> return (chr n)
    coarbitrary n = variant (ord n)
