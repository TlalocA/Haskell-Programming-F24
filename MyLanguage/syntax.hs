module Syntax where

-- grammar of tha language
{-
<program> -> begin <stmts> end
<stmts> -> <stmt> | <stmt>;<stmts>
<stmt> -> <var> = <expr> | print <expr>
<var> -> string
<op> -> + | - | * | / | % | =< | < | > | >= | AND | OR
<expr> -> <val> | <expr> <op> <expr> | if <expr> then <expr> else <expr>
    | func <var> <expr> |  App <expr> <expr> | Ref <var> | Neg <expr> | NegBool <expr>
    | Let <var> <expr> in <expr>
<val> -> integers | double | booleans | String |  error

-- Added: | =< | < | > | >= | AND | OR | --
-- Added | Neg <expr> | NegBool <expr> | Let <var> in <expr> <expr> |--
-- Added: | String | --
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

data Type = TypeI | TypeB | TypeD | TypeS

data Op = Add | Sub | Mul | Div | Mod | Exp | GEq | Gt | LEq | Lt | AND | OR

data Expr = Value Val | BinExpr Expr Op Expr | IfElse Expr Expr Expr | Func Var Type Expr -- expr is the body of the fun
        | App Expr Expr | Ref Var 
        -- negation operator: turns numbers to negative
        | Neg Expr | NegBool Expr
        -- let statement
        | Let Var Expr Expr
    --deriving Show
data Val = ValI Int | ValB Bool | ValD Double | ValE String | ValS String

type Env = [(Var, Val)]

data Result = Valid Env | Error String
    deriving Show

precedence :: Op -> Int
precedence Add = 0
precedence Sub = 0
precedence Mul = 1
precedence Div = 1
precedence Mod = 1
precedence Exp = 2

-- show :: Expr -> String
requiresParenth :: Op -> Expr -> Bool
requiresParenth op (BinExpr _ o2 _) = precedence op < precedence o2 -- 2+3*5
requiresParenth _ _ = False -- 2 + sqr(5)

instance Show Program where
    show (BeginEnd ss) = "begin\n" ++ show ss ++ "\nend"

instance Show Val where
    show (ValI vi) = show vi
    show (ValB vb) = show vb
    show (ValD vd) = show vd
    show (ValS s) = show s
    show (ValE em) = "ERR: " ++ em

instance Show Statements where
    show (End s) = "\t" ++ show s ++ ";"
    show (Seq s ss) = "\t" ++ show s ++ ";\n" ++ show ss 

instance Show Op where 
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"
    show Exp = "**"
    -- =< | < | > | >=
    show GEq = ">="
    show Gt = ">"
    show LEq = "<="
    show Lt = "<"
    -- bool operations
    show AND = "&&"
    show OR = "||"

instance Show Type where
    show TypeI = "int"
    show TypeB = "bool"
    show TypeD = "double"

instance Show Stmt where
    show (Assign x t e) = x ++ ":" ++ show t ++ " = " ++ show e
    show (While e ss) = "while (" ++ show e ++ ")\n" ++ show ss
    show (For s1 e s2 s3) = "for (" ++ show s1 ++ ";" ++ show e ++ ";" ++ show s2 ++ ")\n" ++ show s3
    show (Print e) = "print " ++ show e
    
instance Show Expr where
    show (Value x) = show x
    show (BinExpr el op er) = if requiresParenth op er then
        show el ++ " " ++ show op ++ " (" ++ show er ++ ")"
        else show el ++ " " ++ show op ++ " " ++ show er

    show (IfElse c e1 e2) = "if (" ++ show c ++ ") then " ++ show e1 ++ " else " ++ show e2
    show (Func x t e) = "(" ++ show x ++ ":" ++ show t ++ ") => {" ++ show e ++ "}\n"
    show (App e1 e2) = show e1 ++ "(" ++ show e2 ++ ")"
    show (Ref x) = x
    
    -- show negation
    show (Neg x) = "-(" ++ show x ++ ")"
    show (NegBool x) = "!(" ++ show x ++ ")"

    -- show let
    show (Let x e1 e2) = "let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2

    