module SimpleJSON.PutJSON ( renderJValue ) where

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs ps = intercalate ", " $ pairStrings ps
          pairStrings ps = map renderPair ps
          renderPair (key, value) = show key ++ ": " ++ renderJValue value

renderJValue (JArray a) = "[" ++ values a ++ "]"
    where values [] = ""
          values vs = intercalate ", " $ pairStrings vs
          pairStrings vs = map renderJValue vs


