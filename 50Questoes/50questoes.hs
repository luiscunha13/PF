--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' a b 
    | a>b = []
    | otherwise = a : enumFromTo' (a+1) b

--2
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' s n e 
      | s > e && n >= s || e>s && n<s = []
      | otherwise = s: enumFromThenTo' n (n*2 - s) e

--3
cola :: [a] -> [a] -> [a]
cola [] b = b 
cola (h:t) b = h: cola t b

--4
selec :: [a] -> Int -> a
selec (h:t) l
    | l == 0 =h
    | otherwise = selec (t) (l-1)

--5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]

--6
take' :: Int -> [a] -> [a]
take' n [] = []
take' n (h:t) = [h] ++ take (n-1) t 

--7
drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' 0 l = l
drop' n (h:t) = if n<0 then h:t else drop' (n-1) t

--8
zip' :: [a] -> [b] -> [(a,b)]
zip' a [] = []
zip' [] a = []
zip' (h1:t1) (h2:t2) = [(h1,h2)] ++ (zip' t1 t2)

--9
replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate n a 
    | n<0 = []
    | otherwise = a:(replicate' (n-1) a)

--10
intersperse' :: a -> [a] -> [a]
intersperse' a [] = []
intersperse' _ [a] = [a]
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
concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h++concat' t 

--13
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

--14
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = l : tails'(tail l)

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
fun ((x,y,z):t) = (x,z) : fun t

--18
cola' :: [(String,b,c)] -> String 
cola' [] = ""
cola' ((x,y,z):t) = x ++ cola' t

--19
idade :: Int -> Int -> [(String,Int)] -> [String] 
idade a b [] =[]
idade a b ((n,d):t) 
    | d<=a-b = n : idade a b t 
    |otherwise = idade a b t 

--20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom _ 1 = [1]
powerEnumFrom n m 
    | m>1 = powerEnumFrom n (m-1) ++ [n^(m-1)]
    | otherwise = []

--21
isPrime :: Int -> Bool
isPrime n 
    | n>=2 = verificaprime n 2
    | otherwise = False

verificaprime :: Int -> Int -> Bool
verificaprime n m 
    | m * m > n = True
    | mod n m == 0 = False
    | otherwise = verificaprime n (m + 1)

--22
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (a:b) (h:t) = a == h && isPrefixOf b t 
  
--23
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf a (h:t) = a == h:t || isSuffixOf a t   

--24
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (a:b) (c:d) = a == c && isSubsequenceOf b d || isSubsequenceOf (a:b) d

--25
elemIndices :: Eq a => a -> [a] -> [Int] 
elemIndices _ [] = []
elemIndices a l = posicao a l 0

posicao ::  Eq a => a -> [a] -> Int -> [Int]
posicao _ [] _ = []
posicao a (h:t) i 
    | h == a = i : posicao a t (i+1)
    | otherwise = posicao a t (i+1)

--26
nub :: Eq a => [a] -> [a] 
nub [] = []
nub (h:t) = if h `elem` t then nub t else h:nub t 

--27
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete a (h:t) = if h == a then t else h:delete a t

--28 
remover:: Eq a => [a] -> [a]-> [a] 
remover [] _ = []
remover a [] = a
remover l (h:t) = remover (delete h l) t

--29
union :: Eq a => [a] -> [a] -> [a]
union a [] = a
union [] a = a
union a (h:t) 
    | h `elem` a = union a t
    | otherwise = union (a++[h]) t

--30
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (h:t) l
    | h `elem` l = h:intersect t l
    | otherwise = intersect t l

--31
insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (h:t) 
    | a <=h = a:h:t
    | otherwise = h:insert a t

--32 
unwords' :: [String] -> String
unwords' [] = ""
unwords' (h:t) = h++(if null t then "" else " ")++unwords' t

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
lookup' x ((a,b):t)
    | x == a = Just b 
    | otherwise = lookup' x t

--36
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [a] = [a]
preCrescente (h:c:t)
    |c >= h = h:preCrescente (c:t)
    |otherwise = [h]

--37
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)

--38
menor :: String -> String -> Bool
menor _ "" = False
menor "" _ = True
menor (h:t) (h':t')
    | h< h' = True
    | h == h' = menor t t'
    | otherwise = False

--39
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet x ((a,_):t) = x == a || elemMSet x t

--40
convertMSet :: [(a,Int)] -> [a]
convertMSet [] = []
convertMSet ((a,1):t) = a : convertMSet t
convertMSet ((a,b):t) = a : convertMSet ((a,b-1):t)

--41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((x,y):t) 
    |a == x = (a,y+1) :t
    |otherwise = (x,y): insereMSet a t

--42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = []
removeMSet a ((x,y):t)
    |a == x = if y>1 then (x,y-1):t else t
    |otherwise = (x,y): removeMSet a t

--43
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = insereMSet h (constroiMSet t)

--44
partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers [] = ([], [])
partitionEithers ((Left a):t) = (a:x,y)
        where (x,y) = partitionEithers t 
partitionEithers ((Right b):t) = (x, b:y)
        where (x,y) = partitionEithers t 

--45
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (h:t) = case h of Nothing -> catMaybes t
                            Just h -> h : catMaybes t

--46
data Movimento = Norte | Sul | Este | Oeste
               deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1, y1) (x2,y2)
    |x1 < x2 = Este : caminho ((x1+1), y1) (x2, y2)
    |x1 > x2 = Oeste : caminho ((x1-1), y1) (x2, y2)
    |y1 < y2 = Norte :caminho (x1, (y1+1)) (x2, y2)
    |y1 > y2 = Sul : caminho (x1, (y1-1)) (x2, y2)
    |otherwise = []

--47
posicao' :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao' x [] = x
posicao' (x,y) (h:t) =posicao' (case h of Norte -> (x, y+1)
                                          Sul -> (x, y-1)
                                          Este -> (x+1, y)
                                          Oeste -> (x-1, y) ) t

hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops x t = x == posicao' x t || hasLoops x (init t)

--48
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaquadrados :: [Rectangulo] -> Int
contaquadrados [] = 0
contaquadrados (h:t) 
    | verifquadrado h = 1 +contaquadrados t
    | otherwise = contaquadrados t 

verifquadrado :: Rectangulo -> Bool
verifquadrado (Rect (x1,y1) (x2,y2)) = abs (x1-x2) == abs (y1-y2)

--49
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1) (x2,y2)):t) = abs (x1-x2) * abs (y1-y2) + areaTotal t

--50
data Equipamento = Bom | Razoavel | Avariado
                 deriving Show

naoReparar :: [Equipamento] -> Int 
naoReparar [] = 0
naoReparar (Bom:t) = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t
naoReparar (Avariado:t) = naoReparar t

    
