{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Hspec
import           System.IO.Silently (capture)
import qualified Data.ByteString.Lazy as BL

import           Intermediate.Parser.PrgParser (completePrgParser)
import           Intermediate.Interpreter.Eval (evalPrg)
import           Text.Megaparsec (runParser)

arrayProg1 :: String
arrayProg1 = unlines
  [ "def main() {"
  , "    int* a;"
  , "    a := new int[3];"
  , "    a[0] := 1;"
  , "    a[1] := 2;"
  , "    a[2] := 3;"
  , "    write a[0] + a[1] + a[2];"
  , "    return 0;"
  , "}"
  ]

arrayProg2 :: String
arrayProg2 = unlines
  [ "def main() {"
  , "    int** a;"
  , "    a := new int*[3];"
  , "    int i;"
  , "    while (i < 3) {"
  , "        a[i] := new int[3];"
  , "        int j;"
  , "        j := 0;"
  , "        while (j < 3) {"
  , "            a[i][j] := i * 3 + j;"
  , "            j := j + 1;"
  , "        }"
  , "        i := i + 1;"
  , "    }"
  , "    int sum;"
  , "    sum := 0;"
  , "    int k;"
  , "    k := 0;"
  , "    while (k < 3) {"
  , "        int l;"
  , "        l := 0;"
  , "        while (l < 3) {"
  , "            sum := sum + a[k][l];"
  , "            l := l + 1;"
  , "        }"
  , "        k := k + 1;"
  , "    }"
  , "    write sum;"
  , "}"
  ]

ptrProg :: String
ptrProg = unlines
  [ "def inc(int* a) {"
  , "    a[0] := a[0] + 1;"
  , "    return 0;"
  , "}"
  , "def main() {"
  , "    int* p;"
  , "    p := new int[1];"
  , "    p[0] := 5;"
  , "    inc(p);"
  , "    write p[0];"
  , "    return 0;"
  , "}"
  ]

specEval :: String -> IO String
specEval prgSrc = do
  let parseRes = runParser completePrgParser "inline" prgSrc
  case parseRes of
    Left err -> return $ "ParseError: " ++ show err
    Right prg -> fmap fst $ capture (evalPrg prg)

main :: IO ()
main = hspec $ do
  describe "Interpreter arrays" $ do
    it "computes sum 1+2+3 = 6" $ do
      out <- specEval arrayProg1
      last (lines out) `shouldBe` "6"
    it "computes sum 0+1+2+3+4+5+6+7+8 = 36" $ do
      out <- specEval arrayProg2
      last (lines out) `shouldBe` "36"
    it "increments value through pointer argument" $ do
      out <- specEval ptrProg
      last (lines out) `shouldBe` "6"
