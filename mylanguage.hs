-- grammar of tha language
{-
<program> -> begin <stmts> end
<stmts> -> <stmt> | <stmt>;<stmts>
<stmt> -> <var> = <expr> | print <expr>
<var> -> string
<op> -> + | - | =< | < | > | >= | term
-- term for order of operations
<term> -> * | /
<expr> -> <val> | <term> | <expr> <op> <expr> | if <expr> then <expr> else <expr>
    | func <var> <expr> |  App <expr> <expr> | Ref <var> 
    | do <expr> while <expr> 
-- not sure how to implement <type>?
<val> -> integers | double | booleans | error
-}

newtype Program = BeginEnd Statements
    deriving Show

data Statements = End Stmt | Seq Stmt Statements
    deriving Show

data Stmt = Assign Var Type Expr | While Expr Statements | For Stmt Expr Stmt Statements 
        | Print Expr
    deriving Show

type Var = String

data Type = TypeI | TypeB | TypeD
    deriving Show

data Op = Add | Sub | Mul | Div | GEq | Gt | LEq | Lt | AND | OR
    deriving Show

data Expr = Value Val | BinExpr Expr Op Expr | IfElse Expr Expr Expr | Func Var Type Expr
        | App Expr Expr | Ref Var
    deriving Show

data Val = ValI Int | ValB Bool | ValD Double | ValE String 
    deriving Show

type Env = [(Var, Val)]

data Result = Valid Env | Error String
    deriving Show

evaluateOp:: Int -> Op -> Int -> Val
evaluateOp i1 Add i2 = ValI (i1 + i2)
evaluateOp i1 Sub i2 = ValI (i1 - i2)
evaluateOp i1 Mul i2 = ValI (i1 * i2)
evaluateOp i1 Div i2 = ValI (i1 `div` i2)
evaluateOp i1 GEq i2 = ValB (i1 >= i2)
evaluateOp i1 Gt i2 = ValB (i1 > i2)
evaluateOp i1 LEq i2 = ValB (i1 <= i2)
evaluateOp i1 Lt i2 = ValB (i1 < i2)

evaluateOp:: Bool -> Op -> Bool -> Val
evaluateOp b1 AND b2 = ValI (b1 && b2)
evaluateOp b1 OR b2 = ValI (b1 || b2)

evaluateS :: Stmt -> Env -> Result

evaluateP :: Program -> Result

evaluateSS :: Statements -> Env -> Result

evaluate :: Expr -> Env -> Val

precedence :: Op -> Int
precedence Add = 0
precedence Sub = 0
precedence Mul = 1
precedence Div = 1