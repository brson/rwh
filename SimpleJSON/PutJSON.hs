module SimpleJSON.PutJSON ( renderJValue ) where

import SimpleJSON

renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue _ = undefined