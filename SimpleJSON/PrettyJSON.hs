module SimpleJSON.PrettyJSON ( renderJValue ) where

import SimpleJSON
import SimpleJSON.Prettify

renderJValue :: JValue -> Doc
renderJValue (JString str) = string str
renderJValue (JNumber num) = double num
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JObject objectMembers) = undefined
renderJValue (JArray arrayElements) = undefined
