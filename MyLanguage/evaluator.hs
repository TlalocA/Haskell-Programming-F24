module Evaluator where
import Syntax
import Examples
import System.IO.Unsafe

sequenceSS :: Statements -> Stmt -> Statements
sequenceSS (End s) su = Seq s (End su)
sequenceSS (Seq s1 ss) su = Seq s1 (sequenceSS ss su)

evaluateP :: Program -> Result
evaluateP (BeginEnd ss) = evaluateSS ss []

evaluateSS :: Statements -> Env -> Result
evaluateSS (End s) env = evaluateS s env
evaluateSS (Seq s ss) env = case evaluateS s env of
                                Valid env' -> evaluateSS ss env'
                                Error s -> Error s
    --evaluateSS ss (evaluateS s env)

evaluateS :: Stmt -> Env -> Result
evaluateS (Assign x t e) env = case (evaluate e env, t) of
                                (ValI vi, TypeI) -> Valid ((x, ValI vi) : env)
                                (ValD vd, TypeD) -> Valid ((x, ValD vd) : env)
                                (ValB vb, TypeB) -> Valid $ (x, ValB vb) : env -- bool specific
                                (ValS vs, TypeS) -> Valid ((x, ValS vs) : env) -- for string

                                (ValI _, TypeB) -> Error $ "Type mismatch for " ++ x
                                (ValI _, TypeD) -> Error $ "Type mismatch for " ++ x
                                (ValI _, TypeS) -> Error $ "Type mismatch for " ++ x
                                
                                (ValB _, TypeI) -> Error $ "Type mismatch for " ++ x
                                (ValB _, TypeD) -> Error $ "Type mismatch for " ++ x
                                (ValB _, TypeS) -> Error $ "Type mismatch for " ++ x

                                (ValD _, TypeI) -> Error $ "Type mismatch for " ++ x
                                (ValD _, TypeB) -> Error $ "Type mismatch for " ++ x
                                (ValD _, TypeS) -> Error $ "Type mismatch for " ++ x
                                
                                (ValE s, _) -> Error s
evaluateS (While e ss) env = case evaluate e env of 
                                ValE em -> Error em
                                ValB False -> Valid env
                                ValI 0 -> Valid env
                                _ -> case evaluateSS ss env of
                                        Error em -> Error em
                                        Valid env' -> evaluateS (While e ss) env'
evaluateS (For si e su ss) env = case evaluateS si env of 
                                        Error em -> Error em
                                        Valid env' -> evaluateS (While e (sequenceSS ss su)) env'
evaluateS (Print e) env = case evaluate e env of
                                        ValE em -> unsafePerformIO(print em >> return (Error em))
                                        v -> unsafePerformIO(print v >> return (Valid env))

-- eval for int
evaluateOpInt:: Int -> Op -> Int -> Val
evaluateOpInt i1 Add i2 = ValI (i1 + i2)
evaluateOpInt i1 Sub i2 = ValI (i1 - i2)
evaluateOpInt i1 Mul i2 = ValI (i1 * i2)
evaluateOpInt i1 Div i2 = ValI (i1 `div` i2)
evaluateOpInt i1 Mod i2 = ValI (i1 `mod` i2)
evaluateOpInt i1 Exp i2 = ValI (i1 ^ i2)

-- =< | < | > | >=
evaluateOpInt i1 GEq i2 = ValB (i1 >= i2)
evaluateOpInt i1 Gt i2 = ValB (i1 > i2)
evaluateOpInt i1 LEq i2 = ValB (i1 <= i2)
evaluateOpInt i1 Lt i2 = ValB (i1 < i2)

-- eval for doubles
evaluateOpDoub:: Double -> Op -> Double -> Val
evaluateOpDoub d1 Add d2 = ValD (d1 + d2)
evaluateOpDoub d1 Sub d2 = ValD (d1 - d2)
evaluateOpDoub d1 Mul d2 = ValD (d1 * d2)
evaluateOpDoub d1 Div d2 = ValD (d1 / d2)
evaluateOpDoub d1 Exp d2 = ValD (d1 ** d2)

-- =< | < | > | >=
evaluateOpDoub d1 GEq d2 = ValB (d1 >= d2)
evaluateOpDoub d1 Gt d2 = ValB (d1 > d2)
evaluateOpDoub d1 LEq d2 = ValB (d1 <= d2)
evaluateOpDoub d1 Lt d2 = ValB (d1 < d2)

-- eval for bool
evaluateOpBool:: Bool -> Op -> Bool -> Val
evaluateOpBool b1 AND b2 = ValB (b1 && b2)
evaluateOpBool b1 OR b2 = ValB (b1 || b2)

-- eval for string
evaluateOpString:: String -> Op -> String -> Val
evaluateOpString s1 Add s2 = ValS (s1 ++ s2) -- concatenation

evaluate :: Expr -> Env -> Val
evaluate (Value v) _ = v
evaluate (IfElse c e1 e2) env = case evaluate c env of
                                ValB True -> evaluate e1 env
                                ValB False -> evaluate e2 env
                                ValI 0 -> evaluate e2 env
                                ValI _ -> evaluate e1 env
                                ValE s -> ValE s
                                --ValI i -> error "condition should be a boolean expression"
evaluate (BinExpr e1 op e2) env = case (evaluate e1 env, evaluate e2 env) of
                                (ValI i1, ValI i2) -> evaluateOpInt i1 op i2
                                (ValD d1, ValD d2) -> evaluateOpDoub d1 op d2
                                (ValB b1, ValB b2) -> evaluateOpBool b1 op b2
                                (ValS s1, ValS s2) -> evaluateOpString s1 op s2
                                (ValE em1, _) -> ValE em1
                                (_, ValE em2) -> ValE em2
                                _ -> ValE "operands should be integer"
evaluate (Ref x) env = case lookup x env of
                        Nothing -> ValE "Variable not in scope"
                        Just v -> v
evaluate (App (Func x t e) e2) env = case (evaluate e2 env, t) of
                                        (ValI vi, TypeI) -> evaluate e ((x, ValI vi) : env)
                                        (ValB vb, TypeB) -> evaluate e ((x, ValB vb) : env)
                                        (ValD vd, TypeD) -> evaluate e ((x, ValD vd) : env)

                                        (ValI _, TypeB) -> ValE $ "Type mismatch for " ++ x
                                        (ValI _, TypeD) -> ValE $ "Type mismatch for " ++ x

                                        (ValB _, TypeI) -> ValE $ "Type mismatch for " ++ x
                                        (ValB _, TypeD) -> ValE $ "Type mismatch for " ++ x

                                        (ValD _, TypeI) -> ValE $ "Type mismatch for " ++ x
                                        (ValD _, TypeB) -> ValE $ "Type mismatch for " ++ x

                                        (ValE s, _) -> ValE s

evaluate (Neg e) env = case evaluate e env of
                            ValI vi -> ValI (-vi)
                            ValD vd -> ValD (-vd)
                            ValE em -> ValE em
                            _ -> ValE "Cannot negate value; numerical values only!"

evaluate (NegBool e) env = case evaluate e env of
                            ValB vb -> ValB (not vb)
                            _ -> ValE "Cannot negate value, boolean values only!"

evaluate (Let x e1 e2) env =
                            let val = evaluate e1 env -- declare new env, creats val
                            -- ":" operator essentially adds/binds val to env
                            in evaluate e2 ((x, val) : env) -- changes the value in original env with new env (extends rather than replaces)

evaluate _ env = ValE "undefined" -- Func or App e1 e2 where e1 is not a function

