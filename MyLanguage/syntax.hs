module Syntax where

-- grammar of tha language
{-
<program> -> begin <stmts> end
<stmts> -> <stmt> | <stmt>;<stmts>
<stmt> -> <var> = <expr> | print <expr>
<var> -> string
<op> -> + | - | =< | < | > | >= |
<expr> -> <val> | <expr> <op> <expr> | if <expr> then <expr> else <expr>
    | func <var> <expr> |  App <expr> <expr> | Ref <var> 
    | do <expr> while <expr> 
<val> -> integers | double | booleans | error
-}

newtype Program = BeginEnd Statements

data Statements = End Stmt | Seq Stmt Statements

data Stmt = Assign Var Type Expr
        | While Expr Statements
        | For Stmt Expr Stmt Statements
        | Print Expr
    --deriving Show
-- | IfElS Expr Statements Statements
type Var = String

data Type = TypeI | TypeB | TypeD

data Op = Add | Sub | Mul | Div | GEq | Gt | LEq | Lt | AND | OR

data Expr = Value Val | BinExpr Expr Op Expr | IfElse Expr Expr Expr | Func Var Type Expr -- expr is the body of the fun
        | App Expr Expr | Ref Var
    --deriving Show
data Val = ValI Int | ValB Bool | ValD Double | ValE String 

type Env = [(Var, Val)]

data Result = Valid Env | Error String
    deriving Show

precedence :: Op -> Int
precedence Add = 0
precedence Sub = 0
precedence Mul = 1
precedence Div = 1

instance Show Program where
    show (BeginEnd ss) = "begin\n" ++ show ss ++ "\nend"

instance Show Val where
    show (ValI vi) = show vi
    show (ValB vb) = show vb
    show (ValE em) = "ERR: " ++ em

instance Show Statements where
    show (End s) = "\t" ++ show s ++ ";"
    show (Seq s ss) = "\t" ++ show s ++ ";\n" ++ show ss 

instance Show Op where 
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show GEq = ">="
    show Gt = ">"
    show LEq = "<="
    show Lt = "<"
    show AND = "&&"
    show OR = "||"

instance Show Type where
    show TypeI = "int"
    show TypeB = "bool"
    show TypeD = "double"

instance Show Stmt where
    show (Assign x t e) = x ++ ":" ++ show t ++ " = " ++ show e
    show (While e ss) = "while (" ++ show e ++ ")\n" ++ show ss
    show (For si e su ss) = "for (" ++ show si ++ ";" ++ show e ++ ";" ++ show su ++ ")\n" ++ show ss
    show (Print e) = "print " ++ show 
    
instance Show Expr where
    show (Value x) = show x
    show (BinExpr e Op ei) = show e ++ show Op ++ show ei ++ "\n"
    show (IfElse e ei eu) = "If (" ++ show e ++ ") {" ++ show ei "} Else {" ++ show eu ++ "}\n"
    show (Func x t e) = "(" ++ show x ++ ":" ++ show t ++ ") => {" ++ show e ++ "}\n"
    show (App e ei) = show e ++ show ei -- fix this
    show (Ref x) = "&" ++ show x