poly2 :: Double -> Double -> Double -> Double -> Double
poly2 a b c x = a*x*x + b*x + c

enny:: Integer -> String
enny x = if even x then "eeny" else "meeny"

-- fizzbuzz :: Integer -> String
-- fizzbuzz x | mod x 15 == 0 = "FizzBuzz"
--            | mod x 3 == 0 = "Fizz"
--            | mod x 5 == 0 = "Buzz"
--            | otherwise = ""

fizzbuzz :: Integer -> String
fizzbuzz x = 
    if (mod x 15 == 0) then "FizzBuzz"
    else if (mod x 3 == 0) then "Fizz"
    else if (mod x 5 == 0) then "Buzz"
    else ""


-- fibonacciCazuri :: Integer -> Integer
-- fibonacciCazuri n
--     | n < 2     = n
--     | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)

fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)


-- tribonacci :: Integer -> Integer
-- tribonacci x
--     | x <= 2 = 1
--     | x == 3 = 2
--     | otherwise = tribonacci(x-1) + tribonacci(x-2) + tribonacci(x-3)

tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci n = tribonacci(n-1) + tribonacci(n-2) + tribonacci(n-3)


-- binomial :: Integer -> Integer -> Integer
-- binomial n 0 = 1
-- binomial 0 k = 0
-- binomial n k = binomial (n-1) k + binomial (n-1) (k-1)

binomial :: Integer -> Integer -> Integer
binomial x y
    | x == 0 = 0
    | y == 0 = 1
    | otherwise = binomial(x-1) y + binomial(x-1) (y-1)


func :: [Integer] -> [Integer]
func list@(x:xs) = list

verifL :: [Int] -> Bool
verifL x = even (length x)

takefinal :: [Int] -> Int -> [Int]  -- pt siruri de caractere stergem linia asta
takefinal x y = 
    if length x < y then x
    else drop (length x - y) x 

remove :: [Int] -> Int -> [Int]
remove l n = take (n-1) l ++ drop (n) l

-- semiPareRec [0,2,1,7,8,56,17,18] == [0,1,4,28,9]
semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
 | even h    = h `div` 2 : t'
 | otherwise = t'
 where t' = semiPareRec t

myreplace :: Integer -> Integer -> [Integer]
myreplace 0 v = []
myreplace n v = v: myreplace (n-1) v 

sumImp:: [Int] -> Int
sumImp [] = 0
sumImp (x:xs) = if mod x 2 == 1 then (x + sumImp xs) else sumImp xs

totalLen :: [String] -> Int
totalLen [] = 0
totalLen (x:xs) 
    | head x == 'A' = length x + totalLen xs
    | otherwise = totalLen xs