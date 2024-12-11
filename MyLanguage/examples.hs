module Examples where
import Syntax

--import qualified Syntax as S
e1 :: Expr -- 2 + 3 * 5
e1 = BinExpr (Value (ValI 2)) Add (BinExpr (Value (ValI 3)) Mul (Value (ValI 5))) 

e2 :: Expr -- if (2>=3) then 5 else false
e2 = IfElse (BinExpr (Value (ValI 2)) LEq (Value (ValI 3))) (Value (ValI 5)) (Value (ValB False)) -- less than
--e2 = IfElse (BinExpr (Value (ValI 2)) GEq (Value (ValI 3))) (Value (ValI 5)) (Value (ValB False))

e3:: Expr -- 2
e3 = Value (ValI 2)

e4 :: Expr -- 5 + 3 - 2
e4 = BinExpr (BinExpr (Value (ValI 5)) Add (Value (ValI 3))) Sub (Value (ValI 2))

e5 :: Expr
e5 = BinExpr (BinExpr (Ref "A") Add (Value (ValI 3))) Sub (Value (ValI 2))

e6 :: Expr
e6 = BinExpr (Ref "A") Mul (Ref "A")

e8 :: Expr
e8 = Func "A" TypeI e6

e7 :: Expr
e7 = App e8 (Value (ValB False))

-- AND
b1 :: Expr
b1 = BinExpr (Value (ValB True)) AND (Value (ValB True)) -- True

b2 :: Expr
b2 = BinExpr (Value (ValB True)) AND (Value (ValB False)) -- False

b3 :: Expr
b3 = BinExpr (Value (ValB False)) AND (Value (ValB False)) -- True

-- OR
b4 :: Expr
b4 = BinExpr (Value (ValB True)) OR (Value (ValB True)) -- True

b5 :: Expr
b5 = BinExpr (Value (ValB True)) OR (Value (ValB False)) -- True

b6 :: Expr
b6 = BinExpr (Value (ValB False)) OR (Value (ValB False)) -- False

-- Let
testLet :: Expr
testLet = Let "A" (Value (ValI 7)) (e5)

testConcan :: Expr
testConcan = BinExpr (Value (ValS "Hello")) Add (Value (ValS "World!"))

testString :: Expr
testString = (Value (ValS "Goodbye"))

testNeg :: Expr
testNeg = Neg (Value (ValI 7))

testNegB :: Expr
testNegB = NegBool (Value (ValB True))

d1 :: Expr -- 5.5 + 2.5 - 1.0
d1 = BinExpr (BinExpr (Value (ValD 5.5)) Add (Value (ValD 2.5))) Sub (Value (ValD 1.0))

exp1 :: Expr -- 5.5 + 2.5 - 1.0
exp1 = BinExpr (BinExpr (Value (ValI 10)) Mod (Value (ValI 5))) Sub (Value (ValI 1))

s :: Stmt
s = Assign "X" TypeB b5
-- s = Assign "X" TypeI e7

p1 = BeginEnd (End s)

s1 :: Stmt
s1 = Assign "A" TypeB (Value (ValB True))
-- s1 = Assign "A" TypeI (Value (ValI 5))

s2 :: Stmt
s2 = Assign "B" TypeB (BinExpr (Ref "A") AND (Value (ValB False)))
-- s2 = Assign "B" TypeI (BinExpr (Ref "A") Add (Value (ValI 5)))

s3 :: Stmt
-- s3 = Let "A" (Value (ValB False)) (BinExpr (Ref "A") OR (Ref "B"))
s3 = Assign "C" TypeB (BinExpr (Ref "A") OR (Ref "B"))
-- s3 = Assign "C" TypeI (BinExpr (Ref "A") Add (Ref "B"))

ss :: Statements
ss = Seq s1 (Seq s2 (End (Print e3)))

p :: Program
p = BeginEnd ss

sW :: Stmt -- while A>=3 {print A; A=A-1}
sW = While 
        (BinExpr (Ref "A") GEq (Value (ValI 3))) 
            (Seq 
                (Print (Ref "A")) 
                (End (Assign "A" TypeI (BinExpr (Ref "A") Sub (Value (ValI 1)))))
            )

progW :: Program
progW = BeginEnd (Seq (Assign "A" TypeI (Value (ValI 5))) (End sW))

sF :: Stmt -- for (int B = 5; B >= 3; B = B-1) {Print B}
sF = For 
        (Assign "B" TypeI (Value (ValI 5))) 
        (BinExpr (Ref "B") GEq (Value (ValI 3))) 
        (Assign "B" TypeI (BinExpr (Ref "B") Sub (Value (ValI 1)))) 
            (End (Print (Ref "B")))

progF :: Program
progF = BeginEnd (End sF)

