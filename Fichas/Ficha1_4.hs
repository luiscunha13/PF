module Ficha1 where
import Data.Char (ord, chr)
--4
data Hora = H  Int Int deriving (Show,Eq)

horaValida :: Hora -> Bool
horaValida (H h m) = h >= 0 && h<24 && m >= 0 && m<=59

depoisHora :: Hora -> Hora -> Bool
depoisHora ( H h1 m1) (H h2 m2) 
    | h1<h2 = False
    | h1 == h2 = (m1>m2)
    | h1>h2 = True

--5 a)

data Semaforo = Verde | Amarelo | Vermelho deriving(Show,Eq)

next :: Semaforo -> Semaforo 
next cor 
    |cor == Verde = Amarelo
    |cor == Amarelo = Vermelho
    |otherwise = Verde
--ou
next2 :: Semaforo -> Semaforo 
next2 Verde = Amarelo
next2 Amarelo = Vermelho
next2 Vermelho = Verde

--5 b)
stop :: Semaforo -> Bool
stop cor
    |cor == Vermelho = True
    |otherwise = False

--5 c)
safe :: Semaforo -> Semaforo -> Bool
safe cor cor1 = cor == Vermelho || cor1 == Vermelho


--6 a)
data Ponto = Cartesiano Double Double | Polar Double Double deriving(Show,Eq)

posx :: Ponto ->Double
posx (Cartesiano x _) = x
posx (Polar r a) = r * cos a

--6 b)
posy :: Ponto ->Double
posy (Cartesiano _ y) = y
posy (Polar r a) = r * sin a

--6 c)
raio :: Ponto -> Double
raio (Polar r a) = r
raio (Cartesiano x y) = sqrt (x^2 + y^2)

--6 d)
angulo :: Ponto -> Double
angulo (Polar r a) = a
angulo (Cartesiano x y) = atan(y/x)

--6 e)
dist :: Ponto -> Ponto -> Double
dist (Cartesiano x y) (Cartesiano x2 y2) = sqrt ((x-x2)^2 + (y-y2)^2)
dist (Cartesiano x y) (Polar r a) = sqrt ((x-r*cos a)^2 + (y-r*sin a)^2)
dist (Polar r a) (Polar r1 a1) = sqrt ((r*cos a - r1*cos a1)^2 + (r*sin a -r1*sin a1)^2)
dist (Polar r a) (Cartesiano x y) = sqrt((r*cos a - x)^2 + (r*sin a - y)^2)

--7 a)
--data Figura = Circulo Ponto Double
--            | Retangulo Ponto Ponto
--            | Triangulo Ponto Ponto Ponto deriving(Show, Eq)

--poligono :: Figura -> Bool
--poligono (Circulo c r ) = false
--poligono (Retangulo a b) = posx a /= posx b && posy a /= posy b 
--poligono (Triangulo c d e) =

--7 b)

--8 a)
isLower :: Char -> Bool
isLower x = ord x >= ord 'a' && ord x <= ord 'z'

--8 e)
intToDigit :: Int -> Char
intToDigit x = chr (ord '0' + x)

--8 f)
digitToInt :: Char -> Int
digitToInt x = ord x - ord '0'






 

