module TypeCheck(typeOf) where

import Types (STLCType(..), Context)
import AST (LmExpr(..))
import Var(Var(..))

typeOf :: Context -> LmExpr -> Maybe STLCType
typeOf ctx lmExpr = case lmExpr of
    Variable var -> fromContext ctx var
    TrueLit -> Just Bool
    FalseLit -> Just Bool
    Abstraction (var, t1) body -> do
        t2 <- typeOf ((var, t1):ctx) body
        return (Arrow t1 t2)
    Application e1 e2 -> do
        t1 <- typeOf ctx e1
        t2 <- typeOf ctx e2
        case t1 of
            Arrow argType retType | argType == t2 -> return retType
            _ -> Nothing
    IfThenElse cond left right -> do
        tCond <- typeOf ctx cond
        tLeft <- typeOf ctx left
        tRight <- typeOf ctx right
        if tCond == Bool && tLeft == tRight then 
            return tLeft
        else Nothing


fromContext :: Context -> Var -> Maybe STLCType
fromContext [] _ = Nothing
fromContext ((var, stlcType):xs) vartoSearch = if var==vartoSearch then Just stlcType else fromContext xs vartoSearch