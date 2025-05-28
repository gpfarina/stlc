module Eval (safeEval) where

import AST
import Var
import Types
import TypeCheck

safeEval :: Context -> LmExpr -> Maybe LmExpr
safeEval ctx expr = 
  case typeOf ctx expr of
    Nothing   -> Nothing  -- Ill-typed term, refuse to evaluate
    Just _typ -> Just (eval expr)  -- Well-typed, evaluate normally

eval :: LmExpr -> LmExpr
eval expr = case expr of
  Application e1 e2 ->
    case eval e1 of
      Abstraction (v, _) body -> 
        -- Beta reduction: substitute e2 for v in body
        eval (subst v e2 body)
      e1' -> Application e1' (eval e2)
  Abstraction _ _ -> expr  -- abstractions are values
  Variable _ -> expr       -- variables are values
  TrueLit -> TrueLit -- booleans are values
  FalseLit -> FalseLit -- booleans are values
  IfThenElse cond leftBranch rightBranch ->
    case eval cond of
        TrueLit -> eval leftBranch
        FalseLit -> eval rightBranch
        _ -> error "this cannot happen after typechecking"



subst :: Var -> LmExpr -> LmExpr -> LmExpr
subst (Var x) e1 e2 = -- e1[var |->e2]
    case e1 of
        Variable (Var y) | x==y -> e2
        Variable (Var y) | x/=y -> e1
        Application e3 e4 -> Application (subst (Var x) e3 e2) (subst (Var x) e4 e2)
        Abstraction (Var y, _) _ | x==y -> e1
        Abstraction (Var y, ty) body | x/=y -> if Var y `notElem` freeVars body then
            Abstraction (Var y, ty) (subst (Var x) body e2)
            else
            let v' = freshVar (Var y) (freeVars e2 ++ freeVars body)
                body' = renameVar (Var y) v' body
                in Abstraction (v', ty) (subst (Var y) e2 body')
        _ -> error "Something went wrong"



freeVars :: LmExpr -> [Var]
freeVars expr = case expr of
  TrueLit -> []
  FalseLit -> []
  IfThenElse e1 e2 e3 -> freeVars e1 ++ freeVars e2 ++ freeVars e3 
  Variable v -> [v]
  Application e1 e2 -> freeVars e1 ++ freeVars e2
  Abstraction (v, _) body -> filter (/= v) (freeVars body)



renameVar :: Var -> Var -> LmExpr -> LmExpr
renameVar old new expr = case expr of
  Variable v -> if v == old then Variable new else Variable v
  TrueLit -> TrueLit
  FalseLit -> FalseLit
  IfThenElse e1 e2 e3 -> IfThenElse (renameVar old new e1) (renameVar old new e2) (renameVar old new e3)
  Application e1 e2 -> Application (renameVar old new e1) (renameVar old new e2)
  Abstraction (v, ty) body ->
    if v == old then Abstraction (new, ty) (renameVar old new body)
    else Abstraction (v, ty) (renameVar old new body)

freshVar :: Var -> [Var] -> Var
freshVar (Var base) used = head $ filter (`notElem` used) candidates -- guaranteed to be non empty, so head won't fail
  where candidates = [Var (base ++ show n) | n <- [(1::Int)..]]