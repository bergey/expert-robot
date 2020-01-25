module Main where

import Data.Foldable

import qualified Interpreters.BinInt
import qualified Interpreters.BinDouble
import qualified Compilers.ExprStack

main :: IO ()
main = traverse_ runTests
    [ ("Interpreters.BinDouble", Interpreters.BinDouble.tests)
    , ("Interpreters.BinInt", Interpreters.BinInt.tests)
    , ("Compilers.ExprStack", Compilers.ExprStack.tests)
    ]

runTests :: (String, [String]) -> IO ()
runTests (name, tests) = case tests of
    [] -> putStrLn (name <> " passed")
    errors -> do
        putStrLn (name <> " failed")
        traverse_ putStrLn errors
