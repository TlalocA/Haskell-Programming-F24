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
<val> -> integers | float | booleans |
-}