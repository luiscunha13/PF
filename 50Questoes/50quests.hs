--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' a b 
    | a > b = []
    | otherwise = a : enumFromTo' (a+1) b

--2 
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' a b c
    | (c>a && b<a) || (a>c && b>=a) = []
    | otherwise = a: enumFromThenTo' b (b+(b-a))  c

--3
concat' :: [a] -> [a] -> [a]
concat' [] a = a
concat' (h:t) b = h : concat' t b

--4 
posicao :: [a] -> Int -> a 
posicao (h:t) 0 = h
posicao (h:t) a = posicao t (a-1)

--5
reverse' :: [a] -> [a]
reevrse' [] = []
reverse' (h:t) = reverse' t ++ [h]

--6
take' :: Int -> [a] -> [a]
take' _ [] = []
take' a (h:t)
    | a <=0 = []
    | otherwise = [h] ++ take' (a-1) t

--7
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' a (h:t)
    | a <= 0 = h:t
    | otherwise = drop' (a-1) t

--8 
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (h1:t1) (h2:t2) = (h1,h2) : zip' t1 t2

--9
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' a b 
    | a<0 = []
    | otherwise = b: replicate' (a-1) b

--10
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = [x]
intersperse' a (h:t) = h : a : intersperse' a t

--11
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (h:t) = insere h (group' t)

insere :: Eq a => a -> [[a]] -> [[a]]
insere x [] = [[x]]
insere x (h:t)
    | elem x h = (x:h) : t
    | otherwise = [x] : (h:t)

--12
concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (h:t) = h ++ concat'' t

--13
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

 --14
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = [l] ++ tails' (tail l)

--15 
heads :: [[a]] -> [a]
heads [] = []
heads ([]:t) = heads t
heads (h:t) = head h : heads t

--16
total :: [[a]] -> Int
total [] = 0
total (h:t) = length h + total t

--17
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):t) = (a,c) : fun t

--18
cola :: [(String,b,c)] -> String
cola [] = ""
cola ((a,b,c):t) = a ++ cola t

--19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade a b ((x,y):t) 
    | a - y>= b = x : idade a b t
    | otherwise = idade a b t

--20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 1 = [n]
powerEnumFrom n m 
    | m > 1 = powerEnumFrom n (m-1) ++ [n^(m-1)]
    | otherwise = []

--21

--22
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' a [] = False
isPrefixOf' [] a = True
isPrefixOf' (h:t) (h1:t1) 
    | h == h1 = isPrefixOf' t t1
    | otherwise = False

--23
isSufixOf' :: Eq a => [a] -> [a] -> Bool
isSufixOf' a [] = True
isSufixOf' [] a = False
isSufixOf' a l@(h:t) = a == l || isSufixOf' a t

--24
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] a = True
isSubsequenceOf' a [] = False
isSubsequenceOf' (h:t) (h1:t1) = h == h1 && isSubsequenceOf' t t1 ||isSubsequenceOf' (h:t) t1

--25
{-elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices a [] = []
elemIndices a l = pos a l []

pos :: Eq a => a -> [a] -> [a] -> [Int]
pos -}

--26
{-nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' [x] = [x]
nub' (h:t) | h `elem` t = -}

--27
delete' :: Eq a => a -> [a] -> [a] 
delete' a [] = []
delete' a (h:t) | h == a = t
                | otherwise = h : delete' a t

--28
removeoco :: Eq a => [a] -> [a] -> [a]
removeoco [a] [] = [a]
removeoco [] [a] = []
removeoco (h:t) (h1:t1) | h == h1 = removeoco t t1
                        | otherwise = h:removeoco t (h1:t1)

--29
union' :: Eq a =>  [a] -> [a] -> [a]
union' a [] = a
union' [] a = a
union' l (h1:t1) | h1 `elem` l = union' l t1
                 | otherwise = union' (l++[h1]) t1

--30
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' a [] = a
intersect' [] a = []
intersect' (h:t) l@(h1:t1) | h `elem` l = h:intersect' t l
                           | otherwise = intersect' t l

--31
insert' :: Ord a => a -> [a] -> [a]
insert' a [] = [a]
insert' a (h:t) 
    | a < h = a:h:t
    | otherwise = h: insert' a t

--32
unwords' :: [String] -> String
unwords' [] = ""
unwords' (h:t) = h ++(if null t then "" else " ")++unwords' t

--33
unlines' :: [String] -> String
unlines' [] = ""
unlines' (h:t) = h++"\n"++unlines' t 

--34
pMaior :: Ord a => [a] -> Int
pMaior [_] = 0
pMaior (h:t) 
    | h > (t !! x) = 0
    | otherwise = 1 + x
        where x = pMaior t

--35
lookup' :: Eq a => a -> [(a,b)] -> Maybe b 
lookup' _ [] = Nothing
lookup' a ((x,y):t)
    | a == x = Just y 
    | otherwise = lookup' a t

--36
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente (h:t)
    | head t >= h  = h: preCrescente t
    | otherwise = [h]

--37
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert' h (iSort t)

--38
menor :: String -> String -> Bool
menor _ "" = False
menor "" _ = True
menor (h:t) (h1:t1)
    | h < h1 = True
    | h > h1 = False
    | otherwise = menor t t1

--39
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet a ((x,_):t)= a == x || elemMSet a t

--40
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,1):t) = a : converteMSet t
converteMSet ((a,b):t) = a : converteMSet ((a,b-1):t)

--41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((x,y):t) 
    | a == x = (x,y+1):t
    | otherwise = (x,y):insereMSet a t

--42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet a ((x,y):t)
    | x == a = if y == 1 then t else (x,y-1):t
    | otherwise = (x,y):removeMSet a t

--43
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = insereMSet h (constroiMSet t)

--44
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Left a):t) = (a:x,y)
            where (x,y) = partitionEithers' t 
partitionEithers' ((Right b):t) = (x, b:y)
            where (x,y) = partitionEithers' t

--45
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (h:t) = case h of Nothing -> catMaybes' t 
                             Just h -> h : catMaybes' t

--46
data Movimento = Norte | Sul | Este | Oeste
               deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (x1,y1)
    | x > x1 = Oeste : caminho (x-1,y) (x1,y1)
    | x < x1 = Este : caminho (x+1,y) (x1,y1)
    | y > y1 = Sul : caminho (x,y-1) (x1,y1)
    | y < y1 = Norte : caminho (x,y+1) (x1,y1)
    | otherwise = []

--47
posicao'' :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao'' (x,y) (h:t) = posicao'' ( case h of Norte -> (x,y+1)
                                              Sul -> (x,y-1)
                                              Este -> (x+1,y)
                                              Oeste -> (x-1,y) ) t

hasLoops :: (Int,Int) -> [Movimento] -> Bool   
hasLoops _ [] = False
hasLoops x t = x == posicao'' x t || hasLoops x (init t)

--48
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaquadrados (h:t)
    | verificaquadrado h = 1 + contaQuadrados t
    | otherwise = contaQuadrados t

verificaquadrado :: Rectangulo -> Bool
verificaquadrado (Rect (x,y) (x1,y1)) = abs (x-x1) == abs (y-y1)
    
--49
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal (h:t) = areaquadrado h + areaTotal t

areaquadrado :: Rectangulo -> Float
areaquadrado (Rect (x,y) (x1,y1)) = (abs (x-x1)) * (abs (y-y1)) 

--50
data Equipamento = Bom | Razoavel | Avariado
                 deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Bom:t) = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t 
naoReparar (Avariado:t) = 1 + naoReparar t
 