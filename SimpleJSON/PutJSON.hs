module SimpleJSON.PutJSON ( renderJValue ) where

import SimpleJSON

renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue _ = undefined