data Frac = F Integer Integer
        

--1 a)
mdc :: Integer -> Integer -> Integer
mdc x y 
    | x == y = x
    | x > y = mdc (x-y) y
    | x < y = mdc x (y-x)

normaliza :: Frac -> Frac
normaliza (F x y) = F (s*a) b
    where d = mdc (abs x) (abs y)
          a = div (abs x) d
          b = div (abs y) d 
          s = (signum x) *(signum y)

--1 b)
instance Eq Frac where
    f1 == f2 = (a == x) && (b == y)
        where (F a b) = normaliza f1
              (F x y) = normaliza f2

--1 c)
instance Ord Frac where
    f1 <= f2  = a*y <= x*b
        where (F a b) = normaliza f1
              (F x y) = normaliza f2

--1 d)
instance Show Frac where
    show (F a b) = "("++(show a)++"/"++(show b)++")"

--1 e)
instance Num Frac where
    (F a b) + (F x y) = F (a*y + b*x) (b*y)
    (F a b) * (F x y) = F (a*x) (b*y)
    negate (F a b) = F (-a) b
    abs (F a b) = F (abs a) (abs b)
    signum (F a b) = F ((signum a)*(signum b)) 1
    fromInteger n = F n 1

--1 f)
maioresDobro :: Frac -> [Frac] -> [Frac]
maioresDobro f l = filter (>(2*f)) l


data Exp a = Const Int
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

exp1 = (Mais (Const 3) (Menos (Const 2) (Const 5)))

--2 a)
instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Simetrico a) = "(-"++show a++")"
    show (Mais a b) = "("++show a++" + "++show b++")"
    show (Menos a b) = "("++show a++" - "++show b++")"
    show (Mult a b) = "("++show a++" * "++show b++")"

--2 c)


data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]

--3 a)
instance Ord Data where
    compare (D a b c) (D d e f) | a > b || a == b && (b > e || b == e && c > f) = GT
                                | a == b && b == e && c == f = EQ 
                                | otherwise = LT

--3 b)
instance Show Data where
    show (D a b c) = show a ++"/"++show b++"/"++show c 

--3 c)
ordena :: Extracto -> Extracto
ordena (Ext x ((d,s,m):t))
    |

-- Exemplo da TP

data Ponto = P Float Float 

instance Eq Ponto where
    P x1 y1 == P x2 y2 = x1 == x2 && y1 == y2

instance Ord Ponto where
    P x1 y1 <= P x2 y2 = sqrt (x1^2 + y1^2) <= sqrt (x2^2 + y2^2) -- assumindo que um ponto é menor que o outro se estiver mais próximo da origem

instance Show Ponto where
    show (P x y) = "(" ++ show x ++ "," ++ show y ++ ")" 

instance Num Ponto where
    P x1 y1 + P x2 y2 = P (x1+x2) (y1+y2)
--  ainda faltam definir a *, abs, signum, fromInteger, (negate | (-))

 -- instance (Show a) => (Show BTree a) teria de fazer assim para conseguir definir o show de uma btree porque é necessária primeiramante a (Show a) para mostrar os elementos da árvore

data Genero = Masculino | Feminino | NaoBinario
        deriving Show

data Pessoa = Pessoa { nome :: String, idade :: Int, cc :: Integer, genero :: Genero }

instance Eq Pessoa where
    (==) :: Pessoa -> Pessoa -> Bool
    (==) (Pessoa nome1 idade1 cc1 genero1) (Pessoa nome2 idade2 cc2 genero2) = cc1 == cc2 
    
instance Ord Pessoa where
    compare:: Pessoa -> Pessoa -> Ordering
    compare (Pessoa nome1 idade1 cc1 genero1) (Pessoa nome2 idade2 cc2 genero2) = idade1 `compare` idade2 

instance Show Pessoa where
    show :: Pessoa -> String
    show (Pessoa nome idade cc Masculino) = "Nome: " ++ show nome ++ "\nIdade: "++ (show idade) ++ "\nCC: "++(show cc) ++ "\nGenero: M"
    show (Pessoa nome idade cc Feminino) = "Nome: " ++ show nome ++ "\nIdade: "++ (show idade) ++ "\nCC: "++(show cc) ++ "\nGenero: F"
    show (Pessoa nome idade cc NaoBinario) = "Nome: " ++ show nome ++ "\nIdade: "++ (show idade) ++ "\nCC: "++(show cc) ++ "\nGenero: NB"
