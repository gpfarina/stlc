{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module TypeSpec(spec) where
import Test.Hspec
import Types ( STLCType(..), Var(Var))
import TypeCheck
import AST

spec :: Spec
spec = describe "typeOf" $ do

  it "infers type of a variable from context" $
    typeOf [(Var "x", Unit)] (Variable (Var "x")) `shouldBe` Just Unit

  it "infers type of identity function" $
    let idExpr = Abstraction (Var "x", Unit) (Variable (Var "x"))
    in typeOf [] idExpr `shouldBe` Just (Arrow Unit Unit)

  it "infers type of applied identity function" $
    let idExpr = Abstraction (Var "x", Unit) (Variable (Var "x"))
        app = Application idExpr (Variable (Var "y"))
        ctx = [(Var "y", Unit)]
    in typeOf ctx app `shouldBe` Just Unit

  it "rejects ill-typed application" $
    let f = Abstraction  (Var "x", Arrow Unit Unit) (Variable (Var "y"))
        badApp = Application f (Abstraction (Var "z", Unit) (Variable (Var "z")))
    in typeOf [] badApp `shouldBe` Nothing

  it "typechecks boolean constant, true case" $
    typeOf [] TrueLit `shouldBe` Just Bool

  it "typechecks boolean constant, false case" $
    typeOf [] FalseLit `shouldBe` Just Bool

  it "typechecks boolean expressions, boolean eliminator" $
    typeOf [] (IfThenElse TrueLit TrueLit FalseLit) `shouldBe` Just Bool 

  it "typechecks, boolean expressions with context" $
    typeOf [(Var "x", Bool)] (IfThenElse (Variable (Var "x")) TrueLit FalseLit) `shouldBe` Just Bool 

  it "rejects ill-typed, boolean expressions" $
    typeOf [(Var "x", Unit)] (IfThenElse (Variable (Var "x")) TrueLit FalseLit) `shouldBe` Nothing