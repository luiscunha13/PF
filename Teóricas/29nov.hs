--FULL TREES
-- Árvores binárias que têm informação nos nós intermédios e nas folhas
-- A informação quardada nos nós e nas folhas pode ser de tipo diferente

data FTree a b = Leaf b 
               | No a (FTree a b) (FTree a b) -- não existe empty em full trees (tudo tem informação)
          deriving Show

--OVERLOADING
-- Em Haskell é possível usar o mesmo identificador para funções computacionalmente distintas. A isto chama-se sobrecarga (overloading) de funções
-- Ao nível do sistema de tipos a sobrecarga de funções é tratada introduzindo o conceito de classe e tipos qualificados

-- As classes são uma forma de classificar tipos quanto às funcionalidades que lhe estão associadas
-- Uma classe estabelece um conjunto de assinaturas de funções 
-- Os tipos que são declarados como instâncias dessa classe têm de ter essas funções definidas

-- O tipo principal de uma expressão é o tipo mais geral que lhe é possível associar,de forma a que todas as possíveis instâncias desse tipo constituam ainda tipos válidos para a expressão
-- Toda a expressão válida tem um tipo principal único
-- O Haskell infere sempre o tipo principal de uma expressão

class FigFechada a where -- podemos definir a classe FigFechada
    area :: a -> Float
    perimetro :: a -> Float

areaTotal l = sum (map area l) -- também se pode definir a função areaTotal

--numeros naturais
data Nat = Zero
         | Suc Nat

instance Eq Nat where
    (Suc x) == (Suc y) = x == y
    Zero == Zero = True
    _ == _ = False

tres = Suc (Suc (Suc Zero))    
quatro = Suc tres

-- Ex: tipo de representar as horas
data Time = AM Int Int 
          | PM Int Int 
          | Total Int Int 

minutos :: Time -> Int
minutos (AM h m) = 60 * h + m 
minutos (PM h m) = 12 * 60 + h * 60 + m
minutos (Total h m) = 60 * h + m 

instance Eq Time where
    t1 == t2 = (minutos t1) == (minutos t2)

data BTree a = Empty
             | Node a (BTree a) (BTree a)

instance (Eq a) => Eq (BTree a) where
    Empty == Empty = True
    (Node x1 e1 d1) == (Node x2 e2 d2) = (x1 == x2) && (e1 == e2) && (d2 == d2)
    _ == _ = False


-- Herança
-- O sistema de classes do Haskell tem um mecanismo de herança. Por ex., podemos definir a classe Ord como uma extensão da classe Eq
-- A classe Or herda todas as funções da classe Eq e, além disso, estabelece um conjunto de operações de comparação e as funções ...

instance Ord Nat where
    compare Zero (Suc x) = LT 
    compare Zero Zero = EQ 
    compare (Suc x) Zero = GT
    compare (Suc x) (Suc y) = compare x y

--ou
{-
instance Ord Nat where
    Zero <= _ = True
    (Suc x) <= Zero = False
    (Suc x) <= (Suc y) = x <= y
-}

instance Ord Time where
    t1 <= t2 = minutos t1 <= minutos t2

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Suc n) = 1 + natToInt n

instance Show Nat where
    show n = show (natToInt n) 

instance Show Time where
    show (AM h m) = (show h) ++ (":") ++ (show m) ++ ("am")
    show (PM h m) = (show h) ++ (":") ++ (show m) ++ ("pm")
    show (Total h m) = (show h) ++ ("h") ++ (show m) ++ ("m")

somaNat :: Nat -> Nat -> Nat
somaNat Zero n = n
somaNat (Suc x) n = somaNat x (Suc n)

prodNat :: Nat -> Nat -> Nat
proNat Zero n = Zero
prodNat n Zero = Zero
--prodNat (Suc x) n = somaNat n (ProdNat x n) está a dar erro

instance Num Nat where
    (+) = somaNat
    (*) = prodNat -- está incompleto, falta abs signum e fromInteger
{-
instance Enum Time where
    toEnum n = let (h,m) = divMod n 60
               in Total h m 
    --fromEnum = totalmin falta definir totalmin -}