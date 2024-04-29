{-# LANGUAGE OverloadedStrings #-}

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import Data.Text (pack)
import Control.Monad (void)

-- Define the type of parser errors
type Parser = Parsec Void Text

-- Data structure for propositions
data Prop = Var String | And Prop Prop | Or Prop Prop | Not Prop
    deriving (Show)

-- Parse propositions into structured format
parseProp :: Text -> Either String Prop
parseProp input = case parse propParser "" input of
    Left err -> Left (errorBundlePretty err)
    Right prop -> Right prop

-- Parse a variable name
varParser :: Parser String
varParser = some letterChar

-- Parse a proposition
propParser :: Parser Prop
propParser = andParser <|> orParser <|> notParser <|> varParser'
    where
        -- Parse variables
        varParser' :: Parser Prop
        varParser' = do
            var <- varParser
            return (Var var)
        
        -- Parse conjunction (AND)
        andParser :: Parser Prop
        andParser = do
            p1 <- varParser' <|> notParser
            _ <- space
            _ <- string "&&"
            _ <- space
            p2 <- varParser' <|> notParser
            return (And p1 p2)
        
        -- Parse disjunction (OR)
        orParser :: Parser Prop
        orParser = do
            p1 <- varParser' <|> notParser
            _ <- space
            _ <- string "||"
            _ <- space
            p2 <- varParser' <|> notParser
            return (Or p1 p2)
        
        -- Parse negation (NOT)
        notParser :: Parser Prop
        notParser = do
            _ <- char '!'
            _ <- space
            p <- varParser' <|> notParser
            return (Not p)

-- Evaluate truth value of propositions
evaluate :: Prop -> [(String, Bool)] -> Bool
evaluate (Var x) env = case lookup x env of
    Just val -> val
    Nothing  -> error $ "Variable not found: " ++ x
evaluate (And p1 p2) env = evaluate p1 env && evaluate p2 env
evaluate (Or p1 p2) env = evaluate p1 env || evaluate p2 env
evaluate (Not p) env = not (evaluate p env)

-- Apply inference rules to derive new propositions
applyRule :: Prop -> [Prop]
applyRule prop@(And p1 p2) = [p1, p2]  -- Rule: Conjunction Introduction
applyRule prop@(Or p1 p2) = [p1, p2]    -- Rule: Disjunction Introduction
applyRule prop@(Not (And p1 p2)) = [Not p1, Not p2]  -- Rule: De Morgan's Law
applyRule prop@(Not (Or p1 p2)) = [Not p1, Not p2]    -- Rule: De Morgan's Law
applyRule _ = []  -- No applicable rule

main :: IO ()
main = do
    putStrLn "Welcome to the Interactive Theorem Prover"
    putStrLn "Enter propositions (e.g., 'p && q || !r') or 'quit' to exit"
    interactLoop

interactLoop :: IO ()
interactLoop = do
    putStr "> "
    input <- getLine
    if input == "quit"
        then putStrLn "Exiting..."
        else do
            case parseProp (pack input) of
                Left err -> putStrLn $ "Parsing error: " ++ err
                Right prop -> do
                    putStrLn $ "Parsed proposition: " ++ show prop
                    putStrLn $ "Truth value: " ++ show (evaluate prop [])
                    case applyRule prop of
                        [] -> putStrLn "No inference rule applied"
                        derived -> putStrLn $ "Derived propositions: " ++ show derived
            interactLoop

