module Main where

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import qualified Markup
import qualified Html
import Convert

main :: IO()
main =
    getArgs >>= \ args ->
    case args of
        [] ->
            getContents >>= \ content ->
                putStrLn $ process "No title" content

        [ input, output ] ->
            readFile input >>= \ content ->
                doesFileExist output >>= \ exists ->
                    let
                        writeOut = writeFile output $ process input content
                    in
                        if exists
                            then
                                putStrLn ("Action will overwrite existing file " <> output) *>
                                whenIO confirm writeOut
                            else
                                writeOut

        -- helptext
        _ ->
            putStrLn "Usage: runghc Main.hs [-- input-file output-file]"

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

whenIO condition action =
    condition >>= \ result ->
        if result
            then action
            else pure ()

confirm =
    putStrLn "Confirm [y]es or [n]o:" *>
    getLine >>= \ answer ->
        case answer of
            "y" -> pure True
            "n" -> pure False
            _ ->
                putStrLn "Invalid input" *>
                confirm

