b :: Integer
b = -4.4
c = 4 where a = 3; b = 5
-- comment in line
a = -6 + 1
p :: [Bool]
p = f (b x)
{- 
    block type of comment
-}
f1 :: [Integer]-> Bool -> String
f1 = 2 + b * c
-- comment again
a = f1 ( b  3 3 )
inc a = a + 1
let a = 5; i = 4 in a + 4; w +2
if not ( a > 5 || a < 2) then b + 3 else b + 4
if a <= b && b != a then a * a + b else a / a - b
if True && False then b - 3 else b * 4
add a b = a + b
s = [2,3]
s = [r1,r2]
s = [True,False]
s = [1.4,5.6]


m a = let a = 5; i = 4 in a + 4
m b = if True && False then b + 3 else b + 4
m = case x of
    w -> 12
    c -> 10

numToString x = case x of
  1 -> "jedan"
  2 -> "dva"
  
-- case x of
--     w -> t
--     c -> r
-- case x of
--     w -> 12
--     c -> 10

absoluteDifference a b =
    let diff = if a > b then a - b else b - a
    in diff