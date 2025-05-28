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
    deriving (Eq, Show)