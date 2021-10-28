
factori :: Int -> [Int]
factori n = [x | x <- [1..abs(n)], rem n x == 0]

prim :: Int -> Bool
prim n = if length (factori n) == 2 then True else False

numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2..n], prim x]

myzip3 l1 l2 l3 
    | (null l1) || (null l2) || (null l3) = []
    | otherwise = ((head l1, (head l2), head l3)) : myzip3 (tail l1) (tail l2) (tail l3) 

myzip3' l1 l2 l3 = [(x, y, z) | (x, (y, z)) <- zip l1 (zip l2 l3)]

ordonataNat :: [Int] -> Bool
ordonataNat [] = True
ordonataNat [x] = True
ordonataNat l@(x:xs) = and [a <= b | (a,b) <- zip l xs]

ordonataNat1 :: [Int] -> Bool
ordonataNat1 [] = True
ordonataNat1 [x] = True
ordonataNat1 (x:xs) = ordonataNat1 xs && (x < head xs)

ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata [] rel = True
ordonata [x] rel = True
ordonata (x:y:xs) rel = rel x y && ordonata (y:xs) rel

(*<*) :: (Integer, Integer) -> (Integer, Integer) -> Bool
(x,y) *<* (z,t) = x < z && y < t

compunereList :: (a -> b) -> [b -> c] -> [a ->c]
compunereList fnc lsFnc = [f . fnc | f <- lsFnc]

aplicaList :: a -> [a -> b] -> [b]
aplicaList nr lsFnc = [f nr | f <- lsFnc]