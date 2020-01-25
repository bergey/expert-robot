-- | Compile the two-type expression language to a stack language.
-- Type errors should be caught during compilation, not during
-- execution of the stack program.

module Compilers.DoubleStack where

import qualified Expression.Double as E
import qualified Stack.Double as T

-- | This does no type checking, and does not ever raise errors
unsafeCompile :: E.Expr -> [T.Instruction]
unsafeCompile (E.I i) = [T.Push (T.I i)]
unsafeCompile (E.D x) = [T.Push (T.D x)]
unsafeCompile (E.Op op a b) = op' : unsafeCompile b ++ unsafeCompile a
    where op' = case op of
            E.Add -> T.Op T.Add
            E.Sub -> T.Op T.Sub
            E.Mul -> T.Op T.Mul
            E.Div -> T.Op T.Div

-- | Interleaves type checking with compilation.  Returns the
-- instructions of a stack program, and the type left at the top of
-- the stack after running the program.
safeCompile :: E.Expr -> Either (E.Error E.PrimType) (E.PrimType, [T.Instruction])
safeCompile (E.I i) = Right (E.TyInt, [T.Push (T.I i)])
safeCompile (E.D x) = Right (E.TyDouble, [T.Push (T.D x)])
safeCompile (E.Op op a b) = do
    a' <- safeCompile a
    b' <- safeCompile b
    resultType <- case (op, fst a', fst b') of
        (E.Div, E.TyDouble, E.TyDouble) -> Right E.TyDouble
        (E.Div, aTy, bTy) -> Left (E.IntDivision aTy bTy)
        (_, aTy, bTy) -> if aTy == bTy then Right aTy else Left (E.TypeMismatch aTy bTy)
    let op' = case op of
            E.Add -> T.Add
            E.Sub -> T.Sub
            E.Mul -> T.Mul
            E.Div -> T.Div
    return (resultType, T.Op op' : snd a' ++ snd b')

-- | Also safe, but type checks before compiling instead of
-- interleaving We don't need the PrimType in the result; it's just
-- nice for this to have the same type as @safeCompile@
safeCompile2 :: E.Expr -> Either (E.Error E.PrimType) (E.PrimType, [T.Instruction])
safeCompile2 expr = do
    resultType <- E.typeCheck expr
    return (resultType, unsafeCompile expr)
