import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml = html_
    "austrowgc/lhbg"
    (append_
        (h1_ "Hello, World!")
        (append_
            (p_ "paragraph #1")
            (p_ "paragraph #2")
        )
    )
