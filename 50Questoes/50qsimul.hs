import Data.List
--31
insert' :: Ord a => a -> [a] -> [a]
insert' a [] = []
insert' a (h:t)
    | a < h = a:h:t
    | otherwise = h:insert' a t

--10
intersperce' :: a -> [a] -> [a]
intersperce' a [] = []
intersperce' a [x] = [x]
intersperce' a (h:t) = h:a:intersperce' a t

--6
take' :: Int -> [a] -> [a]
take' a [] = []
take' a (h:t) = [h] ++ take (a-1) t

--37
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)

--49
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x,y) (x1,y1)):t) = abs (x-1) * abs (y-y1) + areaTotal t

--34 dificuldade
pMaior :: Ord a => [a] -> Int
pMaior [_] = 0
pMaior (h:t) 
    | h > (t!!x) = 0
    | otherwise = 1 + x
    where x = pMaior t

--17
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):t) = [(a,c)]++fun t

--50
data Equipamento = Bom | Razoavel | Avariado
                 deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Bom:t) = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t
naoReparar (Avariado:t) = naoReparar t 

--2   dificuldade
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' a b c
    | a<c && b<a  || a>c && b>=a = []
    | otherwise = a: enumFromThenTo' b (b+b-a) c

--25 dificuldade
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices a [] = []
elemIndices a l = posicao a l 0

posicao :: Eq a => a -> [a] -> Int -> [Int]
posicao _ [] _ = []
posicao a (h:t) i 
    | a == h = i : posicao a t (i+1)
    | otherwise = posicao a t (i+1)

--26
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' [a] = [a]
nub' (h:t) = if h `elem` t then nub' t else h:nub' t

--19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade a b [] = []
idade a b ((n,d):t) 
    | d<=a-b = n : idade a b t
    | otherwise = idade a b t

--8
zip' :: [a] -> [b] -> [(a,b)]
zip' a [] = []
zip' [] a = []
zip' (h1:t1) (h2:t2) = (h1,h2) : zip' t1 t2

--42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] 
removeMSet a [] = []
removeMSet a ((b,c):t)
    | a == b = if c >1 then (b,c-1):t else t -- não esquecer esta particularidade
    | otherwise = (b,c) : removeMSet a t

--15
heads' :: [[a]] -> [a]
heads' [] = []
heads' ([]:t) = heads' t               -- não esquecer esta particularidade
heads' (h:t) = head h : heads' t

--40
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,b):t)
    | b == 1 = a : converteMSet t 
    | otherwise = a : converteMSet ((a,b-1):t)

--29
union' :: Eq a => [a] -> [a] -> [a]
union a [] = a  --  não esquecer estes dois casos iniciais
union [] a = a
union' l (h:t)
    | h `elem` l = union' l t
    | otherwise = union' (l++[h]) t 

--28
remover:: Eq a => [a] -> [a] -> [a]
remover [] a = []
remover a [] = a
remover l (h:t) = remover (delete' h l) t

delete' :: Eq a => a -> [a] -> [a] -- esta é a função da alinea 27
delete' a [] = []
delete' a (h:t) = if a==h then t else h:delete' a t

--13  
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]  -- ver melhor depois

--44
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers ((Left a) :t) = (a:x,y)
    where (x,y) = partitionEithers t
partitionEithers ((Right b) :t) = (x,b:y) 
    where (x,y) = partitionEithers t

--41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((b,c):t)
    | a == b = (b,c+1):t
    | otherwise = (b,c) : insereMSet a t

--22
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] a = True
isPrefixOf' a [] = False
isPrefixOf' (h:t) (h1:t1)=  h == h1 && isPrefixOf' t t1

--43
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = insereMSet h (constroiMSet t) -- função da alinea 41

--45
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (h:t) = case h of Nothing -> catMaybes t 
                            Just h -> h : catMaybes t

--38
menor :: String -> String -> Bool
menor "" _ = True
menor _ "" = False
menor (h:t) (h1:t1)
    | h<h1 = True
    | h>h1 = False
    | otherwise = menor t t1

--47
data Movimento = Norte | Sul | Este | Oeste
               deriving Show

posicao' :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao' x [] = x
posicao' (x,y) (h:t) =posicao' (case h of Norte -> (x, y+1)
                                          Sul -> (x, y-1)
                                          Este -> (x+1, y)
                                          Oeste -> (x-1, y) ) t

hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops x t = x == posicao' x t || hasLoops x (init t)

--39
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a ((b,c):t) = a == b || elemMSet a t

--20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom _ 1 = [1]
powerEnumFrom n m 
    | m>1 = powerEnumFrom n (m-1) ++ [n^(m-1)]
    | otherwise = []

--16
total :: [[a]] -> Int
total [] = 0
total (h:t) = length h + total t

--18
cola :: [(String,b,c)] -> String
cola [] = ""
cola ((a,b,c):t) = a++cola t 

--30
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' _ [] = []
intersect' [] _ = []
intersect' (h:t) l
    | h `elem` l = h: intersect' t l 
    | otherwise = intersect' t l

--3 
concat' :: [a] -> [a] -> [a]
concat' [] a = a
concat' (h:t) b = h : concat' t b

--36
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente (h:t)
    | h<=head t = h:preCrescente t
    | otherwise = [h]

--14
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = l : tails' (tail l)

--24
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (h:t) (h1:t1) = h == h1 && isSubsequenceOf t t1 || isSubsequenceOf' (h:t) t1

--33
unlines' :: [String] -> String
unlines' [] = ""
unlines' (h:t) = h ++ "\n" ++ unlines' t

--48
contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados ((Rect (x,y) (x1,y1)):t) 
    | abs (x-x1) == abs (y-y1) = 1 + contaQuadrados t
    | otherwise = contaQuadrados t

--21
isPrime :: Int -> Bool
isPrime n
    | n>= 2 = verificaprime n 2
    | otherwise = False

verificaprime :: Int -> Int -> Bool
verificaprime n m 
    | m*m > n = True
    | mod n m == 0 = False
    | otherwise = verificaprime n (m+1)

--9
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a  
    | n<0 = []
    | otherwise = a : replicate' (n-1) a

--23
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' a l@(h:t) = a == l  || isSuffixOf' a t

--27
delete'' :: Eq a => a -> [a] -> [a]
delete'' a [] = []
delete'' a (h:t)
    | a == h = t 
    | otherwise = h: delete'' a t 

--5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]

--35
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' a [] = Nothing
lookup' a ((b,c):t) 
    | a == b = Just c 
    | otherwise = lookup' a t

--32
unwords' :: [String] -> String
unwords' [] = ""
unwords' (h:t) = h ++(if null t then "" else " ") ++ unwords' t