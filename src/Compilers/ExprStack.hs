-- | Compile a minimal expression language to a stack language.
-- Nothing tricky here - no functions, no data structures, no let
-- bindings.

module Compilers.ExprStack where

import Interpreters.BinInt as S
import qualified Interpreters.Stack as T

import Prelude
import Data.Maybe

compile :: S.Expr -> [T.Instruction]
compile (I i) = [T.Push i]
compile (Op op a b) = compile b ++ compile a ++ [op'] where
  op' = case op of
      S.Add -> T.Op T.Add
      S.Sub -> T.Op T.Sub
      S.Mul -> T.Op T.Mul

equivalent :: String -> Expr -> Maybe String
equivalent err expr = case (S.eval expr, T.eval [] (compile expr)) of
    (s, Right (t : _)) | s == t -> Nothing
                      | otherwise -> Just ( err ++ " " ++ show s ++ " ≠ " ++ show t )
    (_, Left error) -> Just (err ++ " " ++ show error)
    (_, Right []) -> Just (err ++ " empty stack")

tests :: [String]
tests = catMaybes
    [ equivalent "literal" $ I 123
    , equivalent "add" $ Op Add (I 123) (I 444)
    , equivalent "subtract" $ Op Sub (I 456) (I 121)
    , equivalent "nested" $ Op Add (I 123) (Op Add (I 456) (I 789))
    , equivalent "non-associative" $ Op Sub (Op Sub (I 9) (I 4)) (I 3)
    ]
