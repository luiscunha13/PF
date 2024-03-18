import Data.Char (isDigit, isAlpha, intToDigit)

--1 
digitAlpha :: String -> (String,String)
digitAlpha "" = ("","")
digitAlpha (h:t)
        | isAlpha h = (h:alphas,digits)
        | isDigit h = (alphas, h:digits)
        | otherwise = (alphas, digits)
        where (alphas,digits) = digitAlpha t

--função soma com acumulador
soma :: [Int] -> Int
soma l = somaAux 0 l

somaAux :: Int -> [Int] -> Int
somaAux acc [] = acc
somaAux acc (h:t) = somaAux (acc + h) t

--2
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t)
    | h < 0 = (neg+1,zero,pos)
    | h == 0 = (neg,zero+1,pos)
    | h > 0 = (neg,zero,pos+1)
    where (neg,zero,pos) = nzp t


--4
fromDigits :: [Int] -> Int
fromDigits l = fromDigitsAux 0 l

fromDigitsAux :: Int -> [Int] -> Int
fromDigitsAux acc [] = acc
fromDigitsAux acc (h:t) = fromDigitsAux (h + 10*acc) t

--7
intToStr :: Int -> String
intToStr 0 = "0"
intToStr n = intToStrAux "" n

intToStrAux :: String -> Int -> String
intToStrAux acc 0 = acc
intToStrAux acc n = intToStrAux (intToDigit r : acc) q
    where q = n `div` 10
          r = n `mod` 10

intToStr' :: Int -> String
intToStr' 0 = "zero"
intToStr' n = intToStrAux' "" n

intToStrAux' :: String -> Int -> String
intToStrAux' (' ':acc) 0 = acc
intToStrAux' acc 0 = acc
intToStrAux' acc n = intToStrAux' (intToStrName r ++ " " ++ acc) q
    where q = n `div` 10
          r = n `mod` 10

intToStrName :: Int -> String
intToStrName 0 = "zero"
intToStrName 1 = "um"  
intToStrName 2 = "dois"  
intToStrName 3 = "três"  
intToStrName 4 = "quatro"  
intToStrName 5 = "cinco"  
intToStrName 6 = "seis"  
intToStrName 7 = "sete"  
intToStrName 8 = "oito" 
intToStrName 9 = "nome"     

--8 a)
-- [6,12,18]
a = [x | x <- [1..20], mod x 6 == 0]

--8 b)
-- [6,12,18]
b = [x | x <- [6,12,18]]

--8 c)
-- [(10,20), (11,19), (12,18), (13,17), (14,16,), (15,15), (16,14), (17,13), (18,12), (19,11), (20,10)]
c = [(30 - y,y) | y <- [10..20]]

--8 d)
-- [1,1,4,4,9,9,16,16,25,25]
d = [ x ^2 | x <- [1..5], y <- [1..2]]


--9 a)
f = [2^x | x <- [0,1,2,3,4,5,6,7,8,9,10]]

--9 d)
d9 = [ replicate n 1 | n <- [1..5]]
d9' = [ take n [1,1 ..] | n <- [1..5]]

dobros:: Num b => [b] -> [b]
dobros l = map (2*) l

pares:: Integral a => [a] -> [a]
pares l = filter even l

--segundos:: [[b]] -> [b]
--segundos l = map (\x -> (tail x)) l

