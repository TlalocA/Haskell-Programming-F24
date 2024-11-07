-- comments

-- variables
-- Int, Float/Double, String, Bool

-- immutable variables, cannot be redefined in the same scope
x::Int -- specifies datatype: however, haskell _can_ assume datatypes (still good practice)
x = 5 -- `:t x` to get type

y::Float -- specifies datatype: haskell will assume Double for floating point (unless specified)
y = 3.5 -- :r to rerun (recompile)

z::Int
z = 7

w = x+z

-- division, modulo
-- integer division
a = z `div` x -- must be `div` for integer division

b = div z x

c = z `mod` x
-- floating point division: requires both operands to be floating-point
d = y / fromIntegral(x) -- fromIntegral() converts integer to floating

b1::Bool
b1 = True

b2::Bool
b2 = False

s1::String
s1 = "Hello"

s2 = "World!"

-- concatenation operation
s3 = s1 ++ s2

-- relational operators
-- <, >, <=, >=, ==, /=

n1 = 5
n2 = 3

n3 = if n1 == n2 then n1+n2 else n1-n2 --if <expression> then <expression> else <expression>

n4 = if n1 == n2 -- w/ indentation (can help readability)
        then n1 + n2
        else n1 - n2

c3 = case n1 == n2 of 
        True -> n1 + n2
        False -> n1 - n2 -- "_" is default case

n5 = if n1 == n2 then n1+n2 else (if n1 < n2 then n1 else n2) -- parenthesis can also help with readability

n6 = if n1 == n2 -- nested if statements
        then n1+n2
        else if n1 < n2
                then n1
                else n2

c6 = case n1 == n2 of -- nested case statements
        True -> n1+n2
        False -> case n1 < n2 of
                    True -> n1
                    False -> n2

pg -- patterns guards
    |n1 == n2 = n1+n2 -- indentation does matter
    |n1 < n2 = n1
    |n1 > n2 = n2

-- let expressions

v1 = 7
v2 = 4

if1 = if v1 /= v2 then v1+v2 else v1-v2

l1 = let v2 = 10 in if1 -- 11

l3 = let v1 = 10 in let v2 = 10 in if v1 /= v2 then v1+v2 else v1-v2 -- nested let expression
l4 = let (v1, v2) = (10,10) in if v1 /= v2 then v1+v2 else v1-v2