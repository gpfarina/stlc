module Eval (safeEval, subst, renameVar) where

import AST
import Var
import TypeCheck

safeEval :: LmExpr -> Maybe LmExpr
safeEval e = do
  _ <- typeOf [] e
  if isValue e then Just e
  else do
    reduced <- step e
    safeEval reduced


step :: LmExpr -> Maybe LmExpr
step e = 
  case e of
  Application e1 e2
    | isValue e1 && isValue e2 ->
        case e1 of
          Abstraction (v, _) body -> return $ subst v body e2
          _ -> Nothing
    | isValue e1 -> do
        e2' <- step e2
        return $ Application e1 e2'
    | otherwise -> do
        e1' <- step e1
        return $ Application e1' e2
  IfThenElse guard left right
    | guard == TrueLit -> Just left
    | guard == FalseLit -> Just right
    | otherwise -> do
      guard' <- step guard 
      return $ IfThenElse guard' left right
  Succ e' -> do
    e'' <- step e'
    Just $ Succ e''
  NatRec baseCase _ Zero -> return baseCase
  NatRec baseCase (Variable varN, Variable varAcc, stepBody) (Succ n) -> do
      let recCall = NatRec baseCase (Variable varN, Variable varAcc, stepBody) n
          stepSubst1 = subst varN stepBody n
          stepSubst2 = subst varAcc stepSubst1 recCall
          in return stepSubst2
  _ -> error "this should not happen"


isValue :: LmExpr -> Bool
isValue expr = case expr of 
  TrueLit -> True
  FalseLit -> True
  Zero -> True
  Succ e -> isValue e
  Abstraction _ _ -> True
  _ -> False


subst :: Var -> LmExpr -> LmExpr -> LmExpr
subst (Var x) e1 e2 = -- e1[var |->e2], in e1 substitute every occurence of x with e2
    case e1 of
        Variable (Var y) | x==y -> e2
        Variable (Var y) | x/=y -> e1
        Application e3 e4 -> Application (subst (Var x) e3 e2) (subst (Var x) e4 e2)
        Abstraction (Var y, _) _ | x==y -> e1
        Abstraction (Var y, ty) body | x /= y ->
          if Var y `notElem` freeVars e2 then
            Abstraction (Var y, ty) (subst (Var x) body e2)
          else
            let v' = freshVar (Var y) (freeVars e2 ++ freeVars body)
                body' = renameVar (Var y) v' body
            in Abstraction (v', ty) (subst (Var x) body' e2)
        TrueLit -> TrueLit
        FalseLit -> FalseLit
        Zero -> Zero 
        Succ e -> Succ (subst (Var x) e e2)
        NatRec e1' (e2', e3, e4) e5 -> NatRec (subst (Var x) e1' e2) (subst (Var x) e2' e2, subst (Var x) e3 e2, subst (Var x) e4 e2) (subst (Var x) e5 e2)
        _ -> error "Something went wrong"



freeVars :: LmExpr -> [Var]
freeVars expr = case expr of
  TrueLit -> []
  FalseLit -> []
  IfThenElse e1 e2 e3 -> freeVars e1 ++ freeVars e2 ++ freeVars e3 
  Variable v -> [v]
  Application e1 e2 -> freeVars e1 ++ freeVars e2
  Abstraction (v, _) body -> filter (/= v) (freeVars body)
  Zero -> []
  Succ e -> freeVars e
  NatRec e1 (e2, e3, e4) e5 -> freeVars e1 ++ freeVars e2 ++ freeVars e3 ++ freeVars e4 ++ freeVars e5




renameVar :: Var -> Var -> LmExpr -> LmExpr
renameVar old new expr = case expr of
  Variable v ->
    if v == old then Variable new else Variable v
  TrueLit -> TrueLit
  FalseLit -> FalseLit
  Zero -> Zero
  Succ e -> Succ (renameVar old new e)
  NatRec e1 (e2, e3, e4) e5 ->
    NatRec (renameVar old new e1)
           (renameVar old new e2, renameVar old new e3, renameVar old new e4)
           (renameVar old new e5)
  IfThenElse e1 e2 e3 ->
    IfThenElse (renameVar old new e1) (renameVar old new e2) (renameVar old new e3)
  Application e1 e2 ->
    Application (renameVar old new e1) (renameVar old new e2)
  Abstraction (v, ty) body
    | v == old -> Abstraction (new, ty) (renameVar old new body)
    | v == new -> -- prevent capture by renaming bound var first
        let v' = freshVar v (freeVars body ++ [new]) in
        Abstraction (v', ty) (renameVar old new (renameVar v v' body))
    | otherwise -> Abstraction (v, ty) (renameVar old new body)


freshVar :: Var -> [Var] -> Var
freshVar (Var base) used = head $ filter (`notElem` used) candidates -- guaranteed to be non empty, so head won't fail
  where candidates = [Var (base ++ show n) | n <- [(1::Int)..]]