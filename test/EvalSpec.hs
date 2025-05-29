{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module EvalSpec(spec) where
import Test.Hspec
import AST
import Eval (safeEval, subst, renameVar)
import Types ( STLCType(Bool), Var(Var) )  
spec :: Spec
spec = describe "eval" $ do

  it "performs application correctly, value" $
    let abstr = Abstraction (Var "x", Bool) (Variable $ Var "x")
        input = TrueLit
    in safeEval (Application abstr input) `shouldBe` Just TrueLit

  it "performs application correctly, value" $
    let abstr0 = Abstraction (Var "y", Bool) (Variable $ Var "y")
        abstr1 = Abstraction (Var "x", Bool) (Variable $ Var "x")
        input = TrueLit
    in safeEval (Application abstr0 (Application abstr1 input)) `shouldBe` Just TrueLit

  it "performs correct boolean elimination, true case" $ 
    let ifThenElse = IfThenElse TrueLit TrueLit FalseLit
    in  safeEval ifThenElse `shouldBe` Just TrueLit

  it "performs correct boolean elimination, true case complex" $ 
    let abstr0 = Abstraction (Var "y", Bool) (Variable $ Var "y")
        abstr1 = Abstraction (Var "x", Bool) (Variable $ Var "x")
        input = TrueLit
        guard = Application abstr0 (Application abstr1 input)
        ifThenElse = IfThenElse guard TrueLit FalseLit
    in  safeEval ifThenElse `shouldBe` Just TrueLit

  it "performs correct boolean elimination, false case" $ 
    let ifThenElse = IfThenElse FalseLit TrueLit FalseLit
    in  safeEval ifThenElse `shouldBe` Just FalseLit

  it "performs basic nat evaluation" $
    let two   = Succ (Succ Zero)
        three = Succ (Succ (Succ Zero))
        in safeEval (Succ two) `shouldBe` Just three

  it "adds 2 and 3 correctly" $ do
    -- Evaluate add two three
    let zero = Zero
        one = Succ Zero
        two = Succ one
        three = Succ two
        four = Succ three
        five = Succ four
        add x y = NatRec x (Variable $ Var "n", Variable $ Var "acc", Succ (Variable (Var "acc"))) y
        expr = add two three
        result = safeEval expr  -- your eval function
    result `shouldBe` Just five

  it "renames a free variable in a variable expression" $ do
      let e = Variable (Var "x")
      renameVar (Var "x") (Var "y") e `shouldBe` Variable (Var "y")

  it "does not rename if variable does not match" $ do
    let e = Variable (Var "x")
    renameVar (Var "z") (Var "y") e `shouldBe` e

  it "renames variables inside application" $ do
    let e = Application (Variable (Var "x")) (Variable (Var "z"))
    renameVar (Var "x") (Var "y") e `shouldBe`
      Application (Variable (Var "y")) (Variable (Var "z"))

  it "renames bound variable and its occurrences" $ do
    let e = Abstraction (Var "x", Bool) (Variable (Var "x"))
    renameVar (Var "x") (Var "y") e `shouldBe`
      Abstraction (Var "y", Bool) (Variable (Var "y"))

  it "avoids variable capture by renaming bound var" $ do
    -- lambda y. x  renamed x -> y should rename bound y first
    let e = Abstraction (Var "y", Bool) (Variable (Var "x"))
        renamed = renameVar (Var "x") (Var "y") e
    case renamed of
      Abstraction (Var vName, _) body -> do
        vName `shouldNotBe` "y"            -- bound var renamed to avoid capture
        body `shouldBe` Variable (Var "y")     -- free x renamed to y
      _ -> expectationFailure "Expected Abstraction"

  it "substitutes a free variable with an expression" $ do
    let e = Variable (Var "x")
        sub = Variable (Var "z")
    subst (Var "x") e sub `shouldBe` sub

  it "leaves other variables unchanged" $ do
    let e = Variable (Var "y")
        sub = Variable (Var "z")
    subst (Var "x") e sub `shouldBe` e

  it "substitutes in applications" $ do
    let e = Application (Variable (Var "x")) (Variable (Var "y"))
        sub = TrueLit
    subst (Var "x") e sub `shouldBe` Application TrueLit (Variable (Var "y"))

  it "does not substitute inside abstractions that bind the same variable" $ do
    let e = Abstraction (Var "x", Bool) (Variable (Var "x"))
        sub = Variable (Var "z")
    subst (Var "x") e sub `shouldBe` e

  it "substitutes inside abstractions with different variable name and no capture" $ do
    let e = Abstraction (Var "y", Bool) (Application (Variable (Var "x")) (Variable (Var "y")))
        sub =  TrueLit
    subst (Var "x") e sub `shouldBe` Abstraction (Var "y", Bool) (Application TrueLit (Variable (Var "y")))

  it "renames bound variables to avoid capture" $ do
    let e = Abstraction (Var "y", Bool) (Application (Variable (Var "x")) (Variable (Var "y")))
        sub = Variable (Var "y")
        result = subst (Var "x") e sub
    case result of
      Abstraction (Var y', _) body -> do
        y' `shouldNotBe` "y" -- ensure capture avoidance via renaming
        body `shouldBe` Application (Variable (Var "y")) (Variable $ Var y')
      _ -> expectationFailure "Expected Abstraction"