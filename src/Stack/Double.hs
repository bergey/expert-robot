{-# LANGUAGE RankNTypes #-}
-- | A stack language, just barely fancier than the one in Interpreters.Stack.

module Stack.Double where

import Prelude

data PrimValue = I Int | D Double
    deriving (Show, Eq)

data Instruction = Push PrimValue | Pop | Op BinOp
    deriving (Show, Eq)

data BinOp = Add | Sub | Mul | Div
    deriving (Show, Eq)

data Error = Underflow | TypeMismatch PrimValue PrimValue | IntDivision PrimValue PrimValue

type Stack = [PrimValue]

eval :: Stack -> [Instruction] -> Either Error Stack
eval xs (Push x : is) = eval (x : xs) is
eval (_ : xs) (Pop : is) = eval xs is
eval [] (Pop : _) = Left Underflow
eval (x : y : xs) (Op op : is) = do
    r <- f x y
    eval (r : xs) is
  where
    f x y = case op of
        Add -> lift (+) x y
        Sub -> lift (-) x y
        Mul -> lift (*) x y
        Div -> case (x, y) of
            (D x, D y) -> Right (D (x / y))
            _ -> Left (IntDivision x y)
    lift :: (forall a. Num a => a -> a -> a) -> PrimValue -> PrimValue -> Either Error PrimValue
    lift op x y = case (x, y) of
        (I i, I j) -> Right (I (op i j))
        (D x, D y) -> Right (D (op x y))
        _ -> Left (TypeMismatch x y)

eval _ (Op _ : _) = Left Underflow
eval xs [] = return xs

run :: [Instruction] -> Either Error Stack
run = eval []
