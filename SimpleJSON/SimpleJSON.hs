module SimpleJSON (JValue (JString
                          ,JNumber
                          ,JBool
                          ,JNull
                          ,JObject
                          ,JArray)
                  ,getString) where

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