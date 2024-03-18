--1 a) funA [2,3,5,1] = [4,9,25,1]
--1 b) funB [8,5,12] = [8,12]
--1 c) funC [1,2,3,4,5] = [5]
--1 d) funD "otrec" = 

--2 a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = 2*h :(dobros t)

--2 b) 
numOcorre :: Char -> String -> Int
numOcorre x "" = 0
numOcorre x (h:t)
    | x == h = 1 + numOcorre x t
    |otherwise = numOcorre x t

--2 c)
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) 
    |h > 0 = positivos t
    |otherwise = False

--2 d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t)
    |h>0 = h:(soPos t)
    |otherwise = soPos t 

--2 e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t)
    | h<0 = h + somaNeg t 
    | otherwise = somaNeg t 

--2 f)
--tresUlt :: [a] -> [a]
--tresUlt [] = []
--tresUlt (h:t)


--2 g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,x1):t) = [x1]++(segundos t)

--2 i)
sumTriplos :: (Num a,Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):t) = (a+ra,b+rb,c+rc)
    where (ra,rb,rc) = sumTriplos t 

--4 a)
type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n ((coeficiente,grau):t) 
    | grau == n = 1 + conta n t 
    |otherwise = conta n t 

--4 b)
grau :: Polinomio -> Int
grau [] = 0
grau l = maximum (map snd l)

--4 c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n ((c,g):t)
    | g == n = (c,g) : selgrau n t 
    | otherwise = selgrau n t

--4 d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c,g):t) = (c* fromIntegral g,g-1) : deriv t 

deriv' :: Polinomio -> Polinomio
deriv' [] = []
deriv' l = undefined

--4 e)
calcula :: Float -> Polinomio -> Float
calcula x [] = 0
calcula x ((c,g):t) = c * (x^g) + calcula x t

--4 f)
simp :: Polinomio -> Polinomio
simp [] = []
simp ((c,g):t) 
    | c == 0 = simp t 
    | otherwise = (c,g):simp t 

--4 g)
mult :: Monomio -> Polinomio -> Polinomio
mult n [] = []
mult (a,b) ((c,d):t) = (a*c,b+d): mult (a,b) t

--4 h)
normaliza :: Polinomio -> Polinomio 
normaliza [] = []
normaliza ((c,g):t) = inserir (c,g) (normaliza t)

inserir :: Monomio -> Polinomio -> Polinomio
inserir m [] = [m]
inserir (cm,gm) ((c,g):t)
    |g == gm = (c+cm,g) : t 
    |otherwise = (c,g) : inserir (cm, gm) t

--4 i)
soma :: Polinomio -> Polinomio -> Polinomio









