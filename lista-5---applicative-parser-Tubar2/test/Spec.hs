module Main where

import Test.Hspec
import AParser
import SExpr
import Data.Char
import Control.Applicative

main :: IO ()
main = hspec $ do
  testFunctor
  testApplicative
  testParsers1
  testAlternative
  testIntOrUpper
  testParsers2
  testParsers3
  testSExpr

testFunctor = do
  describe "Functor" $ do
    it "Parser Char -> Parser Bool" $ do
      runParser (fmap isLower (char 'c')) "c" `shouldBe` Just (True,"")
    it "Parser Char -> Parser Char" $ do
      runParser (fmap toUpper (char 'C')) "C" `shouldBe` Just('C', "")
    it "Parser Char -> Parser Char" $ do
      runParser (fmap toUpper (char 'C')) "c" `shouldBe` Nothing
    it "Parser Char -> Parser Int" $ do
      runParser (fmap ord (char 'c')) "c" `shouldBe` Just (99,"")
    it "Parser Int -> Parser Char" $ do
      runParser (fmap (intToDigit . fromIntegral) posInt) "4" `shouldBe` Just ('4',"")
    it "Parser Int -> Parser Char" $ do
      runParser (fmap (chr . fromIntegral) posInt) "104" `shouldBe` Just ('h',"")   

testApplicative = do
  describe "Applicative" $ do
    it "Parser (Char, Integer)" $ do
      runParser ((,) <$> (char 'c') <*> (posInt)) "c4" `shouldBe` Just (('c',4),"")
    it "Parser Integer" $ do
      runParser ((+) <$> (fmap (fromIntegral . ord) (char 'c')) <*> (posInt)) "c4" `shouldBe` Just (103,"")
    it "Parser [Char]" $ do
      runParser ((\f s -> [f] ++ [s]) <$> (char 'c') <*> (char 'c')) "cc" `shouldBe` Just ("cc","")
    it "Parser Char" $ do
      runParser ((\f s -> (chr . fromIntegral) (f + s)) <$> (fmap (fromIntegral . digitToInt) (char '5')) <*> (posInt)) "599" `shouldBe` Just ('h',"")
    it "Parser (Integer, Integer)" $ do
      runParser ((,) <$> (posInt) <*> (posInt)) "c4" `shouldBe` Nothing

testParsers1 = do
  describe "Parsers 1" $ do
    it "abParser 1" $ do
      runParser abParser "ab" `shouldBe` Just (('a','b'),"")
    it "abParser 2" $ do
      runParser abParser "ac" `shouldBe` Nothing
    it "abParser_ 1" $ do
      runParser abParser_ "abxy" `shouldBe` Just ((),"xy")
    it "abParser_ 2" $ do
      runParser abParser_ "acxy" `shouldBe` Nothing
    it "intPair 1" $ do
      runParser intPair "23 45" `shouldBe` Just ([23,45],"")
    it "intPair 2" $ do
      runParser intPair "2345" `shouldBe` Nothing

testAlternative = do
  describe "Alternative" $ do
    it "Parser Integer 1" $ do
      runParser (id <$> (posInt <|> (fmap (fromIntegral . ord) (char 'c')))) "4" `shouldBe` Just (4,"")
    it "Parser Integer 2" $ do
      runParser (id <$> (posInt <|> (fmap (fromIntegral . ord) (char 'c')))) "c" `shouldBe` Just (99,"")
    it "Parser Integer 3" $ do
      runParser (id <$> (posInt <|> (fmap (fromIntegral . ord) (char 'c')))) "d" `shouldBe` Nothing
    it "Parser Char 1" $ do
      runParser (id <$> ((fmap (chr . fromIntegral) posInt) <|> (char 'c'))) "c" `shouldBe` Just ('c',"")
    it "Parser Char 2" $ do
      runParser (id <$> ((fmap (chr . fromIntegral) posInt) <|> (char 'c'))) "55" `shouldBe` Just ('7',"")
    it "Parser Char 3" $ do
      runParser (id <$> ((fmap (chr . fromIntegral) posInt) <|> (char 'c'))) "d" `shouldBe` Nothing
            
testIntOrUpper = do
  describe "intOrUpper" $ do
    it "Parser IntOrUpperCase 1" $ do
      runParser intOrUpperCase "42x" `shouldBe` Just ((),"x")
    it "Parser IntOrUpperCase 2" $ do
      runParser intOrUpperCase "x42X" `shouldBe` Nothing
    it "Parser IntOrUpperCase 3" $ do
      runParser intOrUpperCase "X42x" `shouldBe` Just ((),"42x")
    it "Parser IntOrUpperCase 4" $ do
      runParser intOrUpperCase "x42x" `shouldBe` Nothing

testParsers2 = do
  describe "zeroOrMore | oneOrMore" $ do
    it "zeroOrMore 1" $ do
      runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC","dEfgH")
    it "zeroOrMore 2" $ do
      runParser (zeroOrMore (satisfy isLower)) "ABCH" `shouldBe` Just ("","ABCH")
    it "zeroOrMore 3" $ do
      runParser (zeroOrMore (satisfy isUpper)) "ABCDFGH" `shouldBe` Just ("ABCDFGH","")
    it "oneOrMore 1" $ do
      runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC","dEfgH")
    it "oneOrMore 2" $ do
      runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Nothing
    it "oneOrMore 3" $ do
      runParser (oneOrMore (satisfy isUpper)) "AbcdeFGh" `shouldBe` Just ("A","bcdeFGh")

testParsers3 = do
  describe "Parsers 2" $ do
    it "ident 1" $ do
      runParser ident "foobar baz" `shouldBe` Just ("foobar"," baz")
    it "ident 2" $ do
      runParser ident "foobarbaz" `shouldBe` Just ("foobarbaz","")
    it "ident 3" $ do
      runParser ident "2S2Haskell" `shouldBe` Nothing
    it "ident 4" $ do
      runParser ident "" `shouldBe` Nothing
    it "spaces 1" $ do
      runParser spaces " " `shouldBe` Just (" ","")
    it "spaces 2" $ do
      runParser spaces "   foobar" `shouldBe` Just ("   ","foobar")
    it "spaces 3" $ do
      runParser spaces "2S2Haskell" `shouldBe` Just ("", "2S2Haskell")
    it "spaces 4" $ do
      runParser spaces "" `shouldBe` Just ("","")

testSExpr = do
  describe "Parser SExpr" $ do
    it "Parser SExpr 1" $ do
      runParser parseSExpr "5" `shouldBe` Just (A (N 5),"")
    it "Parser SExpr 2" $ do
      runParser parseSExpr "foo3" `shouldBe` Just (A (I "foo3"),"")
    it "Parser SExpr 3" $ do
      runParser parseSExpr "(bar (foo) 3 5 874)" `shouldBe` Just (Comb [A (I "bar"),Comb [A (I "foo")],A (N 3),A (N 5),A (N 874)],"")
    it "Parser SExpr 4" $ do
      runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)" `shouldBe` Just (Comb [Comb [Comb [A (I "lambda"),A (I "x"),Comb [A (I "lambda"),A (I "y"),Comb [A (I "plus"),A (I "x"),A (I "y")]]],A (N 3)],A (N 5)],"")
    it "Parser SExpr 5" $ do
      runParser parseSExpr "( lots of ( spaces in ) this ( one ) )" `shouldBe` Just (Comb [A (I "lots"),A (I "of"),Comb [A (I "spaces"),A (I "in")],A (I "this"),Comb [A (I "one")]],"")

