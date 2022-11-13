-- Html/Internal.hs
module Html.Internal
where

newtype Html = Html String

newtype Structure = Structure String

type Title = String

getHtmlString :: Html -> String
getHtmlString (Html str) = str

getStructureString :: Structure -> String
getStructureString a =
    case a of
        Structure str -> str

el :: String -> String -> String
el tag inner = "<" <> tag <> ">" <> inner <> "</" <> tag <> ">"

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

ul_ :: [Structure] -> Structure
ul_ =  Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

html_ :: Title -> Structure -> Html
html_ title content = Html
    (el "html"
        (el "head" (el "title" (escape title))
            <> el "body" (getStructureString content)
        )
    )

instance Semigroup Structure where
    (<>) c1 c2 =
        Structure $ getStructureString c1 <> getStructureString c2

render :: Html -> String
render html =
    case html of
        Html string -> string

escape :: String -> String
escape =
    let
        escapeChar c =
            case c of
                '<' -> "&lt;"
                '>' -> "&gt;"
                '&' -> "&amp;"
                '"' -> "&quot;"
                '\'' -> "&#39;"
                _ -> [c]
    in
        concatMap escapeChar
