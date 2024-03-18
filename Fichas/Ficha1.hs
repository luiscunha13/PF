-- 1 a)
perimetro :: Double -> Double
perimetro r = 2 * pi * r 

--1 b)
dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1, y1) (x2, y2) = sqrt((x1-x2)^2 + (y1-y2)^2) 

--1 c)
primUlt :: [a] -> (a,a)
primUlt l = (head l,last l)

--1 d)
multiplo :: Int -> Int -> Bool
multiplo m n = mod m n == 0 

--1 e)
truncaImpar :: [a] -> [a]
truncaImpar l = if mod (length l) 2 == 0 then l else tail l

--1 f)
max2 :: Int -> Int -> Int
max2 a b = if a > b then a else b

--1 g)
max3 :: Int -> Int -> Int -> Int
max3 a b c = if max2 a b == a then max2 a c else max2 b c

-- 2 a)
nRaizes :: Float -> Float -> Float -> Int
nRaizes a b c 
    | delta < 0 = 0
    | delta == 0 = 1
    | otherwise = 2
    where delta = b ^ 2 -4 * a * c

-- 2 b)
raizes :: Float -> Float -> Float -> [Float]
raizes a b c 
    | nRaizes a b c == 0 = []
    | nRaizes a b c == 1 = [(-b + sqrt(delta))/2*a]
    | nRaizes a b c == 2 = [(-b + sqrt(delta))/2*a  , (-b - sqrt(delta))/2*a]
    where delta = b ^ 2 -4 * a * a

-- 3 a)
type Hora = (Int,Int)

horaValida :: Hora -> Bool
horaValida (h,m) = h >= 0 && h<24 && m >= 0 && m <= 59

-- 3 b)
depoisHora :: Hora -> Hora -> Bool
depoisHora (h1,m1) (h2,m2) 
    | h1<h2 = False
    | h1 == h2 = (m1>m2)
    | h1>h2 = True

-- 3 c)
converterHoras :: Hora -> Int
converterHoras (h,m) = h * 60 + m 

--3 d)
converterMinutos :: Int -> Hora
converterMinutos m = (div m 60,mod m 60)

--3 e)
diferencaHoras :: Hora -> Hora -> Int
diferencaHoras (h1,m1) (h2,m2) = (h1-h2) * 60 + m1-m2

--3 f)
adminHora :: Int -> Hora -> Hora
adminHora a (h,m)= ()


