module SimpleJSON.Prettify
    ( Doc (..)
    , string
    , text
    , double
    , empty
    , char
    , (<>)
    , compact
    , escapeString
    , oneChar
    , smallHex
    ) where


import Data.Char (ord)
import Numeric (showHex)

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text [] = Empty
text str = Text str

double :: Double -> Doc
double num = text $ show num

string :: String -> Doc
string = enclose '"' '"' . escapeString

-- Wrap a Doc value with opening and closing characters
enclose :: Char -> Char -> Doc -> Doc
enclose left right doc = char left <> doc <> char right

escapeString :: String -> Doc
escapeString s = text s

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith escapePair "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where escapePair char1 char2 = (char1, ['\\', char2])

hexEscape :: Char -> Doc
hexEscape ch = smallHex (ord ch)

smallHex :: Int -> Doc
smallHex i = text "\\u"
             <> text (replicate (4 - length h) '0')
             <> text (showHex i "")
    where h = showHex i ""

-- Append two Doc values, similar to ++
(<>) :: Doc -> Doc -> Doc
Empty <> Empty = Empty
Empty <> doc   = doc
doc   <> Empty = doc
doc1  <> doc2  = doc1 `Concat` doc2

compact :: Doc -> String
compact doc = undefined