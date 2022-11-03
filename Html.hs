module Html
    ( Html
    , Title
    , Structure
    , html_
    , h1_
    , p_
    , append_
    , render
    )
    where

newtype Html = Html String -- the whole document

newtype Structure = Structure String -- a tag and its contents

type Title = String

getHtmlString :: Html -> String
getHtmlString (Html str) = str

getStructureString :: Structure -> String
getStructureString (Structure str) = str

el :: String -> String -> String
el tag inner = "<" <> tag <> ">" <> inner <> "</" <> tag <> ">"

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

html_ :: Title -> Structure -> Html
html_ title content = Html
    (el "html"
        (el "head" (el "title" title)
            <> el "body" (getStructureString content)
        )
    )

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b)

render :: Html -> String
render html =
    case html of
        Html string -> string
