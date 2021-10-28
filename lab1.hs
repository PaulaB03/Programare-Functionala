import Data.List

myInt = 5555555555555555555555555555555555555555555555555555555555555555555555555555555555555

fact n = if n == 0
    then 1
    else n * fact (n-1)
-- functie-> apelare ex: fact 5

--trei = let a = 1; b = 2 in a+b
trei = let
     a = 1
     b = 2
     in a+b
-- trei=3

x = let z = 5; g u = z + u
    in let z = 7
       in g 0 + z
-- x=12

f x = g x + g x + z
  where
       g x = 2 * x
       z = x - 1

h x | x == 0 = 0
    | x == 1 = y + 1
    | x == 2 = y * y
    | otherwise = y
    where y = x * x

c x = case x of
     0 -> 0
     1 -> y + 1
     2 -> y * y
     _ -> y
     where y = x * x

triple x = x * x * x

double :: Integer -> Integer
double x = x+x

maxim :: Integer -> Integer -> Integer
maxim x y = if (x > y) then x else y

max3 x y z = let
             u = maxim x y
             in (maxim  u z)   

-- Exercitiul 6
pow2 :: Integer -> Integer -> Integer
pow2 x y = x * x + y * y

even_odd :: Integer -> String
even_odd x = if (mod x 2 == 0) then "par" else "impar"

double2 :: Integer -> Integer -> Bool
double2 x y = if (x > 2*y) then True else False