import Data.Char

vocale :: String -> Int 
vocale "" = 0
vocale x = 
    if elem (head x) "aeiouAEIOU" then 1 + vocale (tail x)
    else vocale (tail x)

nrVocale :: [String] -> Int 
nrVocale [] = 0
nrVocale (x:xs) 
    | x == reverse x = vocale x + nrVocale xs 
    | otherwise = nrVocale xs

-- f :: Int -> [Int] -> [Int]
-- f a [] = []
-- f a ls
--     | even (head ls) = head ls : a : f a (tail ls) 
--     | otherwise = head ls : f a (tail ls)

f :: Int -> [Int] -> [Int]
f a ls
    | null ls = []
    | even (head ls) = head ls : a : f a (tail ls) 
    | otherwise = head ls : f a (tail ls)

elemPare :: [Int] -> [Int]
elemPare ls = [x | x <- ls, even x]

divizori :: Int -> [Int]
divizori x = [d | d <- [1..x], mod x d == 0]

listadiv :: [Int] -> [[Int]]
listadiv ls = [divizori x | x <- ls]

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec a b ls 
    | null ls = []
    | a <= head ls && head ls <= b = head ls : inIntervalRec a b (tail ls)
    | otherwise = inIntervalRec a b (tail ls)


inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b ls = [x | x <- ls, a <= x && x <= b]

pozitiveRec :: [Int] -> Int 
pozitiveRec ls
    | null ls = 0
    | head ls > 0 = 1 + pozitiveRec (tail ls)
    | otherwise = pozitiveRec (tail ls)

pozitiveComp :: [Int] -> Int 
pozitiveComp ls = sum [1 | x <- ls, x > 0]

pozitiiImpare :: Int -> [Int] -> [Int]
pozitiiImpare poz ls
    | null ls = []
    | odd (head ls) = poz : pozitiiImpare (poz + 1) (tail ls)
    | otherwise = pozitiiImpare (poz + 1) (tail ls)

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec ls = pozitiiImpare 0 ls

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp ls =  [s | x <- zip ls [0..], let (f, s) = x, odd f] 

multDigitRec :: [Char] -> Int 
multDigitRec str 
    | null str = 1
    | isDigit (head str) = (digitToInt (head str)) * multDigitRec (tail str)
    | otherwise = multDigitRec (tail str)

multDigitComp :: [Char] -> Int 
multDigitComp str = product [digitToInt x | x <- str, isDigit x]

{-
[x^2 | x <- [1..10], rem x 3 == 2]      -- [4, 25, 64]
[(x,y) | x <- [1..5], y <- [x..(x+2)]]  -- [(1,1),(1,2),(1,3),(2,2),(2,3),(2,4),(3,3),(3,4),(3,5),(4,4),(4,5),(4,6),(5,5),(5,6),(5,7)]
[(x,y) | x <- [1..3], let k = x^2, y <- [1..k]]     -- [(1,1),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9)]
[x | x<- "Facultatea de Matemetica si Informatica", elem x ['A'..'Z']]  -- "FMI"
[[x..y] | x <- [1..5], y <- [1..5], x<y]    --[[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],[2,3],[2,3,4],[2,3,4,5],[3,4],[3,4,5],[4,5]]
-}

factori :: Int -> [Int]
factori n = [x | x <- [1..abs(n)], rem n x == 0] 
