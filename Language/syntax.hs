module Syntax where 

-- grammar of the language
{-
<program> -> begin <stmts> end
<stmts> -> <stmt> | <stmt>;<stmts>
<stmt> -> <var> = <expr> | while <expr> <stmts> | for <stmt> <expr> <stmt> <stmts> | print <expr>
<var> -> string
<op> -> + | - | * | / | >=
<expr> -> <val> | <expr> <op> <expr> | if <expr> then <expr> else <expr>
    | func <var> <expr> | <expr> <expr> | <var>
<val> -> integers | booleans | error
<env> -> [(<var>, <val>)]
<result> -> <env> | error
-}

-- abstract data types

--data Program = BeginEnd Statements
newtype Program = BeginEnd Statements
    deriving Show

data Statements = End Stmt | Seq Stmt Statements
    deriving Show

data Stmt = Assign Var Type Expr
        | While Expr Statements
        | For Stmt Expr Stmt Statements
        | Print Expr
-- | IfEls Expr Statements Statements
    deriving Show
type Var = String

data Type = TypeI | TypeB
    deriving Show

data Op = Add | Sub | Mul | Div | GEq
    deriving Show

data Expr = Value Val | BinExpr Expr Op Expr | IfElse Expr Expr Expr | Func Var Type Expr -- expr is the body of the fun
        | App Expr Expr | Ref Var
    deriving Show
data Val = ValI Int | ValB Bool | ValE String 
    deriving Show

type Env = [(Var, Val)]

data Result = Valid Env | Error String
    deriving Show

precedence :: Op -> Int
precedence Add = 0
precedence Sub = 0
precedence Mul = 1
precedence Div = 1