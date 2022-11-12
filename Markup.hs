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
    | OrdereedList [String]
    | CodeBlock [String]
    deriving Show

parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
    let
        paragraph = Paragraph (unlines (reverse currentParagraph))
    in
        case txts of
            [] -> [paragraph]
            currentLine : rest ->
                if trim currentLine == ""
                    then paragraph : parseLines [] rest
                    else parseLines (currentLine : currentParagraph) rest

trim :: String -> String
trim = unwords . words

print :: Show a => a -> IO ()
print = putStrLn . show
