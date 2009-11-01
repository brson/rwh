module SimpleJSON.Prettify
    ( Doc
    , string
    , text
    , double
    ) where


data Doc = ToBeDefined
           deriving (Show, Eq)

string :: String -> Doc
string str = ToBeDefined

text :: String -> Doc
text str = ToBeDefined

double :: Double -> Doc
double num = ToBeDefined