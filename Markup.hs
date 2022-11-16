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

parse = parseLines Nothing . lines

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
