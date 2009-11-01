module SimpleJSON.PrettyJSON ( renderJValue ) where

import SimpleJSON
import SimpleJSON.Prettify

renderJValue :: JValue -> Doc
renderJValue (JString str) = undefined
renderJValue (JNumber num) = undefined
renderJValue (JBool True) = undefined
renderJValue (JBool False) = undefined
renderJValue JNull = undefined
renderJValue (JObject objectMembers) = undefined
renderJValue (JArray arrayElements) = undefined
