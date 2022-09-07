main :: IO ()
main = putStrLn html

newtype Html = Html String

newtype Elem = Elem String

getHtmlString :: Html -> String
getHtmlString (Html str) = str

getElemString :: Elem -> String
getElemString (Elem str) = str

el :: String -> String -> String
el tag inner =
  "<" <> tag <> ">" <> inner <> "</" <> tag <> ">"

p_ :: String -> String
p_ = el "p"

h1_ :: String -> String
h1_ = el "h1"

title_ :: String -> String
title_ = el "title"

body_ :: String -> String
body_ = el "body"

head_ :: String -> String
head_ = el "head"

html_ :: String -> String
html_ = el "html"

doc :: String -> String -> String
doc title body = html_ (head_ (title_ title) <> body_ body)

html :: String
html =
  doc
    "austrowGC/lhbg"
    ((h1_ "Hello, World!") <> p_ "Learning Haskell Blog Generator")
