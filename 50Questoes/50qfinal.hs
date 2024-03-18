import Data.List
--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' a b 
    | a>b = []
    | otherwise = a:enumFromTo' (a+1) b 

--2
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' a b c
    | a>c && b>=a || a<c && b<a = []
    | otherwise = a: enumFromThenTo' b (b*2-a) c

--3
concat' :: [a] -> [a] -> [a]
concat' a [] = a
concat' [] a = a
concat' (h:t) l = h:concat' t l

--4
pos :: [a] -> Int -> a
pos (h:t) a
    | a == 1 = h
    | otherwise = pos t (a-1)

--5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]

--6
take' :: Int -> [a] -> [a]
take' _ [] = []
take' a (h:t) = [h] ++ take' (a-1) t

--7
drop' :: Int -> [a] -> [a]
drop' a [] = []
drop' 0 l = l
drop' a (h:t) = if a<0 then h:t else drop' (a-1) t

--8
zip' :: [a] -> [b] -> [(a,b)]
zip' a [] = []
zip' [] a = []
zip' (h:t) (h1:t1) = (h,h1) : zip' t t1

--9
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' a b 
    | a <0 = []
    | otherwise = b: (replicate' (a-1) b)

--10
intersperse' :: a -> [a] -> [a]
intersperse' a [] = []
intersperse' a [x] = [x]
intersperse' a (h:t) = h:a:(intersperse' a t)

--11
group' :: Eq a => [a] -> [[a]]
group' [] =[]
group' (h:t) = insere h (group' t)


insere :: Eq a => a -> [[a]] -> [[a]]
insere x [] = [[x]]
insere x (h:t)
    | elem x h = (x:h) : t
    | otherwise = [x] : (h:t)

--12
concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (x:t) = x ++ concat'' t

--13
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits'  (init l) ++ [l]

--14
tails' :: [a] -> [[a]]
tails' [] =[[]]
tails' l = [l] ++ tails' (tail l)

--15
heads' :: [[a]] -> [a]
heads' [] = []
heads' ([]:t) = heads' t
heads' (h:t) = head h : heads' t

--16
total :: [[a]] -> Int
total [] = 0
total (h:t) = length h + total t

--17
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):t) = (a,c):fun t

--18
cola :: [(String,b,c)] -> String
cola [] = ""
cola ((a,b,c):t) = a ++ cola t

--19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade a b [] = []
idade a b ((c,d):t)
    | a-b >= d = c : idade a b t
    | otherwise = idade a b t

--20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 1 = [1]
powerEnumFrom n m 
    | m>1 = powerEnumFrom n (m-1) ++ [n^(m-1)]
    | otherwise = []

--21
isPrime :: Int -> Bool
isPrime n 
    | n >= 2 = verificaprime n 2
    | otherwise = False

verificaprime :: Int -> Int -> Bool
verificaprime n m 
    | m*m > n = True
    | mod n m == 0 = False
    | otherwise = verificaprime n (m+1)

--22
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (h:t) (h1:t1) = h == h1 && isPrefixOf' t t1

--23
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' l (h:t) = l == h:t || isSuffixOf' l t

--24
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (h:t) (h1:t1) = h == h1 && isSubsequenceOf' t t1 || isSubsequenceOf' (h:t) t1

--25
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' a l = posicao' a l 0

posicao' :: Eq a => a -> [a] -> Int -> [Int]
posicao' a (h:t) n
    | a == h = n : posicao' a t (n+1)
    | otherwise = posicao' a t (n+1)

--26
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' [x] = [x]
nub' (h:t) 
    | h `elem` t = nub' t
    | otherwise = h : nub' t

--27
delete' :: Eq a => a -> [a] -> [a]
delete' a [] = []
delete' a (h:t) = if a == h then t else h:delete' a t

--28
remover :: Eq a => [a] -> [a] -> [a]
remover a [] = a 
remover [] a = []
remover (h:t) (h1:t1)
    | h == h1 = remover t (h1:t1)
    | otherwise = h : remover (h:t) t1

--29
union' :: Eq a => [a] -> [a] -> [a]
union' [] a = a
union' a [] = a 
union' l (h:t) = if h `elem` l then union' l t else  union' (l++[h]) t

--30
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' (h:t) l
    | h `elem` l = h:intersect' t l 
    | otherwise = intersect' t l

--31
insert' :: Ord a => a -> [a] -> [a]
insert' a [] = [a]
insert' a (h:t)
    | a < h = a : h : t
    | otherwise = h : insert' a t 

--32
unwords' :: [String] -> String
unwords' [] = ""
unwords' (h:t) = h ++ (if null t then "" else " ")++ unwords' t 

--33
unlines' :: [String] -> String
unlines' [] = ""
unlines' (h:t) = h ++ "\n" ++ unlines' t 

--34
pMaior :: Ord a => [a] -> Int
pMaior [_] = 0
pMaior (h:t)
    | h > (t !! x) = 0
    | otherwise = 1 + x 
    where x = pMaior t

--35
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' a [] = Nothing
lookup' a ((b,c):t)
    | a == b = Just c
    | otherwise = lookup' a t 

--36
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [a] = [a]
preCrescente (h:t)
    | h <= head t = h: preCrescente t 
    | otherwise = [h]

--37
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort [a] = [a]
iSort (h:t) = insert h (iSort t)

--38
menor :: String -> String -> Bool
menor "" _ = True
menor _ "" = False
menor (h:t) (h1:t1)
    | h == h1 = menor t t1
    | h < h1 = True
    | otherwise = False 

--39
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a ((b,c):t) = a == b || elemMSet a t

--40
convertMSet :: [(a,Int)] -> [a]
convertMSet [] = []
convertMSet ((a,1):t) = a : convertMSet t
convertMSet ((a,b):t) = a : convertMSet ((a,b-1):t)

--41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((b,c):t)
    | a == b = (b,c+1):t
    | otherwise = (b,c) : insereMSet a t 

--42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = []
removeMSet a ((b,c):t)
    | a == b = if c>1 then (b,c-1):t else t 
    | otherwise = (b,c) : removeMSet a t

--43
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = insereMSet h (constroiMSet t)

--44
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers ((Left a):t) = (a:x,y)
            where (x,y) = partitionEithers t 
partitionEithers ((Right b):t) = (x,b:y)
            where (x,y) = partitionEithers t 

--45
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (h:t) = case h of Nothing -> catMaybes t 
                            Just b -> b: catMaybes t

--46
data Movimento = Norte | Sul | Este | Oeste
               deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (x1,y1) 
    | x > x1 = Oeste : caminho ((x-1),y) (x1,y1)
    | x < x1 = Este : caminho ((x+1),y) (x1,y1)
    | y > y1 = Sul : caminho (x,(y-1)) (x1,y1)
    | y < y1 = Norte : caminho (x,(y+1)) (x1,y1)

--47
hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops x l = x == pos' x l || hasLoops x (init l)

pos' :: (Int,Int) -> [Movimento] -> (Int,Int)
pos' (x,y) (h:t) = pos' (case h of Norte -> (x,y+1)  
                                   Sul -> (x,y-1)
                                   Este -> (x+1,y)
                                   Oeste -> (x-1,y) ) t

--48
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados ((Rect (x,y) (x1,y1)):t)
    | abs (x-x1) == abs (y-y1) = 1 + contaQuadrados t 
    | otherwise = contaQuadrados t 

--49
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x,y) (x1,y1)):t) = abs (x-x1) * abs (y-y1) + areaTotal t 

--50
data Equipamento = Bom | Razoavel | Avariado
                 deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Bom:t) = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t
naoReparar (Avariado:t) = naoReparar t


