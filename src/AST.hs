module AST(LmExpr(..)) where

import Types(Binding)
import Var(Var(..))

data LmExpr =
    Variable Var 
    | Application LmExpr LmExpr
    | Abstraction Binding LmExpr 
    deriving (Eq, Show)