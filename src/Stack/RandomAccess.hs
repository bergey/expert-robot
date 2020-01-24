-- | A stack machine that allows reading & deleting locations deep in
-- the stack, so we don't need to read values in (reverse) the order
-- that we wrote them.

module Stack.RandomAccess where

import Prelude
import Data.Word (Word)

-- | Delete generalizes Pop
data Instruction = Push Int | Load Word | Delete Word | Op BinOp
    deriving (Show, Eq)

data BinOp = Add | Sub | Mul
    deriving (Show, Eq)

data Error = Underflow
    deriving Show

type Stack = [Int]

eval :: [Instruction] -> Stack -> Either Error Stack
eval (Push x : is) xs = eval is (x : xs)
eval (Load n : is) xs = do
    x <- nth n xs
    eval is (x : xs)
eval (Delete n : is) xs = do
    xs' <- deleteNth n xs
    eval is xs'
eval (Op op : is) (x : y : xs) = eval is (f x y : xs) where
  f = case op of
      Add -> (+)
      Sub -> (-)
      Mul -> (*)
eval (Op _ : _) _ = Left Underflow
eval [] xs = return xs

nth :: Word -> [a] -> Either Error a
nth _ [] = Left Underflow
nth 0 (a : _) = Right a
nth index (_ : as) = nth (index - 1) as

deleteNth :: Word -> [a] -> Either Error [a]
deleteNth _ [] = Left Underflow
deleteNth 0 (_ : as) = Right as
deleteNth index (a : as) = fmap (a :) (deleteNth (index - 1) as)
