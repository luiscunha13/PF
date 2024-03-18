import Data.List
dobros l = map (2*)

pares l = filter even l

segundos l = map (\x -> head (tail x)) l

--1 a)
any' :: (a-> Bool) -> [a] -> Bool
any' a [] = True
any' a (h:t)
    | a h = True
    | otherwise = any' a t 

--1 b)
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (h:t) (h1:t1) = (f h h1):zipWith' f t t1

--1 f)
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f a [] = []
deleteBy' f a (h:t)
    | f a h = t
    | otherwise = h: deleteBy' f a t 
    
--2 a)
type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = filter (\(c,g) -> g == n) p

--2 b)
conta :: Int -> Polinomio -> Int 
conta n p = length $ filter (\x -> n == snd x) p

--2 c)
grau :: Polinomio -> Int
grau p = maximum (map snd p)

--2 d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c,g):t)
    | g == 0 = deriv t 
    | otherwise = (c * fromIntegral g, g-1): deriv t

deriv' :: Polinomio -> Polinomio
deriv' p = [(c * fromIntegral g, g-1) | (c,g) <- p,g /= 0]

deriv'' :: Polinomio -> Polinomio
deriv'' p = map (\(c,g) -> (c * fromIntegral g,g-1)) $ filter (\(c,g) -> g /= 0) p

--2 e)
calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula n ((c,g):t) = c * (n^g) + calcula n t 

calcula' :: Float -> Polinomio -> Float
calcula' n p = foldr (\(c,g) acc -> acc + (c * (n^g))) 0 p --podemos cortar os 'p' dos dois lados

calcula'' :: Float -> Polinomio -> Float
calcula'' n p = foldl (\acc (c,g) -> acc + (c * (n^g))) 0 p

--3 a)
type Mat a = [[a]]

dimOK :: Mat a -> Bool
dimOK m = (== 1) $ length $ nub $ map length m 

dimOK_feia :: Mat a -> Bool
dimOK_feia m = (length (nub (map length m)) == 1)

dimOK_ponto :: Mat a -> Bool
dimOK_ponto = (== 1) . length . nub . map length

dimOK' :: Mat a -> Bool
dimOK' (h:t) = all (\l -> length l == length h) t

minhadim :: Mat a -> Bool
minhadim l@(h:t) = length (filter (==length h) (map length t)) == length l -1

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat (h:t) (h1:t1) = somamat h h1 : addMat t t1

somamat :: Num a => [a] -> [a] -> [a]
somamat [] [] = []
somamat (h:t) (h1:t1) = (h+h1): somamat t t1



-- foldr (\x acc -> x + acc) 0 [1,2,3,4]
-- função que soma os elementos da direita para a esquerda
-- a função foldr é mais eficiente que a foldl

soma :: [Int] -> Int
soma l = foldr (\x acc -> x+acc) 0 l --também se pode cortar os 'l' dos dois lados

soma' :: [Int] -> Int
soma' = foldr (+) 0