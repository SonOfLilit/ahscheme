-- AHScheme 0
-------------
-- Written by Aur Saraf
--
-- Based on the "Write yourself a Scheme in 48 hours in Haskell" tutorial, but not
-- blindly copied.
--
-- Placed in the Public domain

module Main where
import List
import Monad
import Text.ParserCombinators.Parsec
import Test.HUnit

main :: IO ()
main = do
    _ <- runTestTT unitTests
    return ()

unitTests = test [ parserTests ]

parsesTo :: String -> String -> Test
parsesTo = (~=?) . parseToStr

parsesToIdentity = join parsesTo

parseError :: String -> Test
parseError lisp = let message = "No parse error parsing: " ++ lisp
                      isError = isPrefixOf "\"Error: " (parseToStr lisp)
                  in test $ assertBool message isError

parseToStr :: String -> String
parseToStr = show . readExpression

parserTests = test [ "parse a number" ~: map parsesToIdentity ["0", "1", "26092"],
                     "parse a string" ~: parsesToIdentity "\"Hello World!\"",
                     "parse an atom" ~: map parsesToIdentity ["anAtom", "a5", "!", "a!", "~a#&##^$"] ++ [parseError "5a"],
                     "parse a list" ~: [parsesToIdentity "(a b c)",
                     "(a b ( c d 5) )" `parsesTo` "(a b (c d 5))"]]

data Value = Atom String
           | List [Value]
           | Number Integer
           | String String
           deriving Eq

instance Show Value where show (Atom name) = name
                          show (List list) = "(" ++ (unwords . (map show)) list ++ ")"
                          show (Number value) = show value
                          show (String value) = "\"" ++ value ++ "\""

a <* b = do x <- a; b; return x
a *> b = do a; x <- b; return x

readExpression :: String -> Value
readExpression expr = case parse (parseValue <* eof) "lisp" expr of
    Left err -> String ("Error: " ++ show err)
    Right val -> val

parseValue :: Parser Value
parseValue = parseAtom
          <|> parseList
          <|> parseNumber
          <|> parseString

symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

parseAtom :: Parser Value
parseAtom = do
    let ls = letter <|> symbol
    first <- ls
    rest <- many (ls <|> digit)
    let name = first : rest
    return $ Atom name

spaces0 = many space

parseList :: Parser Value
parseList = do
    char '('
    spaces0
    first <- parseValue
    rest <- many (spaces *> parseValue <* spaces0)
    char ')'
    return . List $ [first] ++ rest

parseNumber :: Parser Value
parseNumber = do
    digits <- many1 digit
    return . Number $ read digits

parseString :: Parser Value
parseString = do
    let q = char '"'
    q
    contents <- many $ noneOf "\""
    q
    return $ String contents
