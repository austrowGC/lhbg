opentag name = "<" <> name <> ">"

closetag name = "</" <> name <> ">"

markup tag text = (opentag tag) <> (text <> clostag tag)

body content = markup "body" content

html content = markup "html" content

main = putStrLn html body "Hello, World!"
