module SimpleJSON.PutJSON ( renderJValue ) where

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull = "null"

renderJValue (JObject objectMembers) = "{" ++ renderMembers objectMembers ++ "}"
    where
      renderMembers :: [JObjectMember] -> String
      renderMembers objectMembers = intercalate ", " $ memberStrings objectMembers

      memberStrings :: [JObjectMember] -> [String]
      memberStrings objectMembers = map renderMember objectMembers

      renderMember :: JObjectMember -> String
      renderMember (key, value) = show key ++ ": " ++ renderJValue value

renderJValue (JArray arrayElements) = "[" ++ renderElements arrayElements ++ "]"
    where
      renderElements :: [JValue] -> String
      renderElements arrayElements = intercalate ", " $ elementStrings arrayElements

      elementStrings :: [JValue] -> [String]
      elementStrings arrayElements = map renderJValue arrayElements

