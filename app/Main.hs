module Main where

import Text.Megaparsec
import           Control.Monad.Trans.Except (runExceptT)
import Intermediate.Parser.PrgParser (completePrgParser)
import Intermediate.Interpreter.Eval (evalPrg)
-- import Typed.Eval (evalPrg)
import Data.Aeson (encodeFile)

main :: IO ()
main = do
    putStrLn "Type file to parse:"
    fileName <- getLine
    putStrLn "Type file to save json:"
    fileNameJson <- getLine
    content <- readFile fileName

    putStrLn content
    let ans = runParser completePrgParser "" content
    case ans of
        Left err ->
            putStrLn $ errorBundlePretty err
        Right expr -> do
            encodeFile fileNameJson expr
            evalPrg expr
            -- pr1 <- runExceptT (compilePrg expr) 
            -- case pr1 of 
            --     Left err -> print err 
            --     Right compiled -> 
            --         evalPrg compiled 

