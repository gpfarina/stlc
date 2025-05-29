module AST(LmExpr(..)) where

import Types(Binding)
import Var(Var(..))

data LmExpr =
    Variable Var 
    | TrueLit
    | FalseLit
    | IfThenElse LmExpr LmExpr LmExpr
    | Application LmExpr LmExpr
    | Abstraction Binding LmExpr
    | Zero
    | Succ LmExpr
    -- | NatRec LmExpr (LmExpr, LmExpr, LmExpr) LmExpr -- baseCase (what to return when we hit Zero), Step function as triple, nat argument
    deriving (Eq, Show)