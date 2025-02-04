module Main where

import Text.Megaparsec
import           Control.Monad.Trans.Except (runExceptT)
import Intermediate.Parser.PrgParser (completePrgParser)
import Typed.Compile (compilePrg)
import Typed.Eval (evalPrg)

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
        Right expr -> do
            pr1 <- runExceptT (compilePrg expr) 
            case pr1 of 
                Left err -> print err 
                Right compiled -> evalPrg compiled 

