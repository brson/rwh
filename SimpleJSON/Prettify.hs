module SimpleJSON.Prettify
    ( Doc (..)
    , string
    , text
    , double
    , empty
    , char
    , (<>)
    , compact
    ) where


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
string = enclose '"' '"' . text

-- Wrap a Doc value with opening and closing characters
enclose :: Char -> Char -> Doc -> Doc
enclose left right doc = char left <> doc <> char right

-- Append two Doc values, similar to ++
(<>) :: Doc -> Doc -> Doc
Empty <> Empty = Empty
Empty <> doc   = doc
doc   <> Empty = doc
doc1  <> doc2  = doc1 `Concat` doc2

compact :: Doc -> String
compact doc = undefined