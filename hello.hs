import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml = html_
    "<whoa>'austrowgc/lhbg'</whoa>"
    (append_
        (h1_ "Hello, World!")
        (append_ myorderedlist mylist)
    )

myorderedlist :: Structure
myorderedlist = ol_ [p_ "gravy", p_ "potatoes"]

mylist :: Structure
mylist = ul_
    [ code_ "myorderedlist :: Structure\nmyorderedlist = ol_ [p_ \"gravy\", p_ \"potatoes\"]\n"
    , code_ "main :: IO ()\nmain = putStrLn (render myhtml)\n"
    ]
