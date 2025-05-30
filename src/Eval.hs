module Eval (safeEval, subst, renameVar) where

import AST
import Var
import TypeCheck
import Types (STLCType(Unit))

safeEval :: LmExpr -> Maybe LmExpr
safeEval e = do
  _ <- typeOf [] e
  eval e

eval:: LmExpr -> Maybe LmExpr
eval e = do
  if isValue e then Just e
  else do
    reduced <- step e
    eval reduced

step :: LmExpr -> Maybe LmExpr
step e =
  case e of
  Application e1 e2
    | isValue e1 && isValue e2 ->
        case e1 of
          Abstraction (v, _) body -> return $ subst v e2 body
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
  Succ e' 
    | not (isValue e') -> do
        e'' <- step e'
        return $ Succ e''
    | otherwise -> Nothing

  NatRec baseCase _ Zero -> return baseCase

  NatRec baseCase (Variable varN, Variable varAcc, stepBody) (Succ n) -> do
    let
      stepFn = Abstraction (varN, Unit) (Abstraction (varAcc, Unit) stepBody) -- we use Unit, types do not matter anymore at this point
      recCall = NatRec baseCase (Variable varN, Variable varAcc, stepBody) n
      appliedToN = Application stepFn n
      appliedToRecCall = Application appliedToN recCall
      in 
      return appliedToRecCall

  _ -> error $ "step: unhandled case: " ++ show e


isValue :: LmExpr -> Bool
isValue expr = case expr of 
  TrueLit -> True
  FalseLit -> True
  Zero -> True
  Succ e -> isValue e
  Abstraction _ _ -> True
  _ -> False

subst :: Var -> LmExpr -> LmExpr -> LmExpr
subst (Var x) target expr =
  case expr of
    Variable (Var y) | x == y -> target
    Variable _                -> expr
    Application e1 e2         -> Application (subst (Var x) target e1) (subst (Var x) target e2)
    Abstraction (Var y, ty) body
      | x == y               -> expr
      | Var y `notElem` freeVars target ->
          Abstraction (Var y, ty) (subst (Var x) target body)
      | otherwise ->
          let v' = freshVar (Var y) (freeVars target ++ freeVars body)
              body' = renameVar (Var y) v' body
          in Abstraction (v', ty) (subst (Var x) target body')

    TrueLit -> TrueLit
    FalseLit -> FalseLit
    Zero -> Zero
    Succ e -> Succ (subst (Var x) target e)
    NatRec e1 (Variable (Var n), Variable (Var acc), eBody) e2
      | x == n || x == acc -> 
          NatRec (subst (Var x) target e1) (Variable (Var n), Variable (Var acc), eBody) (subst (Var x) target e2)
      | Var n `notElem` freeVars target && Var acc `notElem` freeVars target ->
          NatRec (subst (Var x) target e1) (Variable (Var n), Variable (Var acc), subst (Var x) target eBody) (subst (Var x) target e2)
      | otherwise ->
          let n' = freshVar (Var n) (freeVars target ++ freeVars eBody)
              acc' = freshVar (Var acc) (freeVars target ++ freeVars eBody ++ [n'])
              eBody' = renameVar (Var acc) acc' (renameVar (Var n) n' eBody)
          in NatRec (subst (Var x) target e1) (Variable n', Variable acc', subst (Var x) target eBody') (subst (Var x) target e2)
    _ -> error $ "subst: unhandled case: " ++ show expr


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