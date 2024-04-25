import Test.HUnit

-- Import the module containing the arithmetic expression algorithm
-- saved in the file arithmetic.hs in the same directory. The module is called Arithmetic.
import Arithmetic

-- Test cases for the arithmetic expression algorithm
tests :: Test
tests = TestList
    [ testEvaluate "Basic addition (integers)" "2 + 3" 5
    , testEvaluate "Basic subtraction (integers)" "5 - 3" 2
    , testEvaluate "Basic multiplication (integers)" "2 * 3" 6
    , testEvaluate "Basic division (integers)" "6 / 3" 2
    , testEvaluate "Mixed operations (integers)" "2 + 3 * 4 - 10 / 2" 9
    , testEvaluate "Parentheses (integers)" "(2 + 3) * 4" 20
    , testEvaluate "Nested parentheses (integers)" "((2 + 3) * 4) - 5" 15
    , testEvaluate "Basic addition (doubles)" "2.5 + 3.5" 6.0
    , testEvaluate "Basic subtraction (doubles)" "5.5 - 3.5" 2.0
    , testEvaluate "Basic multiplication (doubles)" "2.5 * 3.5" 8.75
    , testEvaluate "Basic division (doubles)" "6.0 / 3.0" 2.0
    , testEvaluate "Mixed operations (doubles)" "2.5 + 3.0 * 4.5 - 10.0 / 2.0" 11.0
    , testEvaluate "Parentheses (doubles)" "(2.5 + 3.5) * 4.0" 24.0
    , testEvaluate "Nested parentheses (doubles)" "((2.5 + 3.5) * 4.0) - 5.0" 19.0
--    , testError "Division by zero" "1 / 0"
--    , testError "Invalid expression" "2 +"
    ]

-- Helper function to create a test case for evaluating an expression
testEvaluate :: String -> String -> Double -> Test
testEvaluate name input expected = TestCase $ do
    assertEqual name expected (calculate input)

-- Helper function to create a test case for expecting a parsing error
--testError :: String -> String -> Test
--testError name input = TestCase $ do
--    let result = calculate input
--    assertBool name (isError result)
--  where
--    isError (Left _) = True
--    isError (Right _) = False

main :: IO ()
main = do
    counts <- runTestTT tests
    putStrLn $ show counts

