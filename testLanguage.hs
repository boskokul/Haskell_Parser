b :: Integer
b = -6 + 1
c = a + b where a = 3; b = 5
-- comment in line
{- 
    block type of comment
-}
f1 :: [Integer]-> Bool -> String
f1 b c= b * c
-- comment again
p :: [Bool]
p = f1 (2 3)
add a b = a + b
inc a = a + 1
incAlt a = add (a 1)
s1 = [1.4,5.6]
s2 = [2,3]
s3 = [r1,r2]
s4 = [True,False]
m2 b = if True && False then b + 3 else b + 4

numToString x = case x of
  1 -> "jedan"
  2 -> "dva"
  
maxOfTwo a b= 
    if a >= b
    then a
    else b

numberNegative :: Integer -> Bool
numberNegative n =
    if n < 0
        then True
        else False

cubePerimeter :: Integer -> Integer
cubePerimeter edgeLength =
    let
        numberOfEdges = 4
    in
        edgeLength * numberOfEdges


