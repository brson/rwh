module SimpleJSON ( JValue ( JString
                           , JNumber
                           , JBool
                           , JNull
                           , JObject
                           , JArray)
                  , getString
                  , getInt
                  ) where

-- | An algebraic data type to represent the range of possible JSON types.
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber n) = Just $ truncate n
getInt _           = Nothing