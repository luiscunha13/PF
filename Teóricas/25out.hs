import Data.Char
-- lista por compreensão [2*x | x <- [10,3,7,2]] que equivale a [20,6,14,4]

-- outras lista [x^2 | x <- [1,2,3,4,5], 10 <= x^2]

fun :: Int -> Int -> [Int]
fun a b = [a..b]

qsort :: (Ord a ) => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort [y | y<-xs, y<x]) ++[x]++ (qsort [y | y<-xs, y>=x])

posicoes :: Eq a => a -> [a] -> [Int]
posicoes a l = [ n | (y,n) <- zip l [0..], y==a]

divisores :: Integer -> [Integer]
divisores n = [ x | x <- [1..n],n `mod` x == 0]

primo :: Integer -> Bool --verifica se um número é primo
primo n = divisores n == [1,n]

--primosAte :: Integer -> [Integer]
--primosAte n = [ x | x <- ]

--crivo :: Int -> [Int]




{-fatorizacao :: Integer -> [Integer]
fatorizacao n = factAux n primos 
    where factAux 1 _ = []
          factAux n (x:xs) | mod n x == 0 = x : factAux (n `div` x) (x:xs)
                           | otherwise = factAux n xs -}

--Funções de ordem superior

mult :: Int -> Int -> Int
mult x y = x *y

triplo :: Int -> Int
triplo = mult 3

triplos :: [Int] -> [Int]
triplos [] = []
triplos (x:xs) = 3*x : triplos xs

maiusculas :: String -> String
maiusculas [] = []
maiusculas (x:xs) = toUpper x : maiusculas xs

somapares :: [(Float,Float)] -> [Float]
somapares [] = []
somapares ((a,b):xs) = a+b : somapares xs

--map :: (a -> b) -> [a] -> [b]
--map f [] = []
--map f (x:xs) = f x : map f xs

triplos' :: [Int] -> [Int]
triplos' l = map (3*) l

maiusculas' :: String -> String
maiusculas' l = map toUpper l

somapares' :: [(Float,Float)] -> [Float]
somapares' l = map soma l
        where soma (x,y) = x+y

pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) = if even x then x: pares xs else pares xs

positivos :: [Double] -> [Double]
positivos [] = []
positivos (x:xs)
    | x>0 = x : positivos xs
    | otherwise = positivos xs

--filter :: (a -> Bool) -> [a] -> [a]
--filter p [] = []
--filter p (x:xs) 
--    | p x = x : filter p xs
--    | otherwise = filter p xs

pares' :: [Int] -> [Int]
pares' l = filter even l

positivos' :: [Double] -> [Double]
positivos' l = filter (>0) l

-- Funções anónimas

-- \x -> x+x
-- escreve-se (\x -> x+x) 5 e devolve 10
somapares'' :: [(Float,Float)] -> [Float]
somapares'' = map (\(x,y) -> x+y) l

trocapares :: [(a,b)] -> [(b,a)]
trocapares l = map (\(x,y)->(y,x)) l


