{-|
Module      : Arithmetic
Description : Module to process and evaluate arithmetic expressions
Author      : Oliver Marx

This module provides functions to process and evaluate arithmetic expressions,
including handling of operators precedence.
-}
module Arithmetic (
    calculate,
) where

-- | Check if a character is a digit
isDigit :: Char -> Bool
isDigit c = elem c "0123456789."

-- | Check if a character is a whitespace character
isSpace :: Char -> Bool
isSpace c = elem c " \t\n"

-- Function to process an arithmetic expression
calculate :: String -> Double
calculate x = evaluate (process ([], []) filtx)
    where
        filtx = filter (not . isSpace) x

-- | Process the arithmetic expression recursively by stacking terms and operators
process :: ([Double], String) -> String -> ([Double], String)
process termsOps expr = case parseTerm expr of
    (term, []) -> (term:terms, operators)
    (term, op:rest) -> process (term:terms, op:operators) rest
    where
        (terms, operators) = termsOps

-- | Evaluate the arithmetic expression by calculating the result of the terms and operators in order of precedence
evaluate :: ([Double], String) -> Double
evaluate ([], _) = error "Empty expression"
evaluate ([x], []) = x
evaluate (terms, operators)
    | elem '*' operators || elem '/' operators =
        let (terms', operators') = processMulDiv terms operators
        in evaluate (terms', operators')
    | otherwise =
        let (terms', operators') = processAddSub terms operators
        in evaluate (terms', operators')

-- | Process the multiplication and division operators in the arithmetic expression
processMulDiv :: [Double] -> String -> ([Double], String)
processMulDiv terms ops = processMulDiv' terms ops [] []
  where
    processMulDiv' :: [Double] -> String -> [Double] -> String -> ([Double], String)
    processMulDiv' [x] [] accTerms accOps = (reverse (x:accTerms), reverse accOps)
    processMulDiv' (term2:term1:rest) ('*':ops) accTerms accOps =
        processMulDiv' ((term1 * term2) : rest) ops accTerms accOps
    processMulDiv' (term2:term1:rest) ('/':ops) accTerms accOps =
        processMulDiv' ((term1 / term2) : rest) ops accTerms accOps
    processMulDiv' (term:restTerms) (op:restOps) accTerms accOps =
        processMulDiv' restTerms restOps (term:accTerms) (op:accOps)
    processMulDiv' _ _ _ _ = error "Invalid expression"

-- | Process the addition and subtraction operators in the arithmetic expression
processAddSub :: [Double] -> String -> ([Double], String)
processAddSub terms ('+':operators) =
    let (term2:term1:rest) = terms
    in processAddSub ((term1 + term2):rest) operators
processAddSub terms ('-':operators) =
    let (term2:term1:rest) = terms
    in processAddSub ((term1 - term2):rest) operators
processAddSub (t:terms) (op:operators) =
    processAddSub terms operators
processAddSub [x] [] = ([x], [])
processAddSub _ _ = error "Invalid expression"

-- | Parse a term (number or subexpression)
parseTerm :: String -> (Double, String)
parseTerm ('(':xs) = 
    let (expr, rest) = parseExpr ([], xs)
    in ((calculate expr), rest)
parseTerm (c:xs)
    | isDigit c || c == '.' =
        let (num, rest) = span isDigit (c:xs)
        in (read num, rest)
    | otherwise = error $ "Unexpected character: " ++ [c]
parseTerm [] = error "Empty expression"

-- | Parse an arithmetic expression
parseExpr :: (String, String) -> (String, String)
parseExpr ("(",[]) = error "Open parenthesis without closing parenthesis"
parseExpr (expr, ')':rest) = (expr, rest)
parseExpr (expr, '(':rest) =
    let (subexpr, rest') = parseExpr ([], rest)
    in parseExpr (expr ++ show (calculate subexpr), rest')
parseExpr (expr, c:rest) = parseExpr (expr ++ [c], rest)
parseExpr _ = error "Invalid expression"

