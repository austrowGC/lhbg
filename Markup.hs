module Markup
    ( Document
    , Structure(..)
    , print
    )
    where
import Numeric.Natural
import Prelude hiding (print)
import Data.Semigroup
import Data.Maybe

type Document
    = [Structure]

data Structure
    = Heading Natural String
    | Paragraph String
    | UnorderedList [String]
    | OrderedList [String]
    | CodeBlock [String]
    deriving (Show, Eq)

parse :: String -> [Structure]
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
    case txts of
        [] -> maybeToList context

        -- Heading 1
        ( '*' : ' ' : line ) : rest ->
            maybe id (:) context $ Heading 1 (trim line) : parseLines Nothing rest

        -- Unordered List
        ( '-' : ' ' : line ) : rest ->
            case context of
                Just (UnorderedList list) ->
                    parseLines (Just $ UnorderedList $ list <> [trim line]) rest
                _ ->
                    maybe id (:) context $ parseLines (Just $ UnorderedList [trim line]) rest

        -- Ordered List
        ( '#' : ' ' : line ) : rest ->
            case context of
                Just (OrderedList list) ->
                    parseLines (Just $ OrderedList $ list <> [trim line]) rest
                _ ->
                    maybe id (:) context $ parseLines (Just $ OrderedList [trim line]) rest

        -- Code Block
        ( '>' : ' ' : line ) : rest ->
            case context of
                Just (CodeBlock list) ->
                    parseLines (Just $ CodeBlock $ list <> [line]) rest
                _ ->
                    maybe id (:) context $ parseLines (Just $ CodeBlock [line]) rest

        -- Paragraph
        currentLine : rest ->
            let
                line = trim currentLine
            in
                if line == ""
                    then
                        maybe id (:) context (parseLines Nothing rest)
                    else
                        case context of
                            Just (Paragraph paragraph) ->
                                parseLines (Just $ Paragraph $ unwords [paragraph, line]) rest
                            _ ->
                                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words

print :: Show a => a -> IO ()
print = putStrLn . show

safeHead :: [a] -> Maybe a
safeHead list =
    case list of
        [] -> Nothing
        x : _ -> Just x

exactlyTwo :: [a] -> Maybe (a, a)
exactlyTwo list =
    case list of
        [x, y] -> Just (x, y)
        _ -> Nothing

-- example data --

ex1 :: Document
ex1 =
    [ Paragraph "Hello, World!"
    ]

ex2 :: Document
ex2 =
    [ Heading 1 "Welcome"
    , Paragraph "To this tutorial about Haskell"
    ]

ex3 :: Document
ex3 =
    [ Paragraph ("something something remember " <> "multiple lines or something")
    , OrderedList
        [ "Item 1 of a list"
        , "Item 2, same list"
        ]
    ]

ex4 :: Document
ex4 =
    [ Heading 1 "Compiling programs with ghc"
    , Paragraph (
        concat
            [ "Running ghc invokes the Glasgow Haskell Compiler (GHC), "
            , "and can be used to compile Haskell modules and programs into native "
            , "executables and libraries."
            ]
    )
    , Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:"
    , CodeBlock
        [ "main = putStrLn \"Hello, Haskell!\""
        ]
    , Paragraph "Now, we can compile the program by invoking ghc with the file name:"
    , CodeBlock
        [ "➜ ghc hello.hs"
        , "[1 of 1] Compiling Main             ( hello.hs, hello.o )"
        , "Linking hello ..."
        ]
    , Paragraph "GHC created the following files:"
    , UnorderedList
        [ "hello.hi - Haskell interface file"
        , "hello.o - Object file, the output of the compiler before linking"
        , "hello (or hello.exe on Microsoft Windows) - A native runnable executable."
        ]
    , Paragraph "GHC will produce an executable when the source file satisfies both conditions:"
    , OrderedList
        [ "Defines the main function in the source file"
        , "Defines the module name to be Main, or does not have a module declaration"
        ]
    , Paragraph "Otherwise, it will only produce the .o and .hi files."
    ]
