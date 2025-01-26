module Main where

import Text.Megaparsec
import PrgParser (completePrgParser)

main :: IO ()
main = do
    putStrLn "Type file to parse:"
    fileName <- getLine
    content <- readFile fileName

    putStrLn content
    let ans = runParser completePrgParser "" content
    case ans of
        Left err ->
            putStrLn $ errorBundlePretty err
        Right expr ->
            print expr

