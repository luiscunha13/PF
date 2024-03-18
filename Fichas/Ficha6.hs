-- função de ordem superior da catmaybes
catMaybes_os :: [Maybe a] -> [a]
catMaybes_os l = foldr (\x acc -> case x of Just y -> y : acc; Nothing -> acc) [] l 

data BTree a = Empty
             | Node a (BTree a) (BTree a)
          deriving Show

--1 a)
altura :: BTree a -> Int
altura Empty = 0
altura (Node _ e d) = 1 + max (altura e) (altura d)

--1 b)
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contanodos (Node _ e d) = 1 + contaNodos e + contaNodos d 

--1 c)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1 
folhas (Node x e d) = folhas e + folhas d

--1 d)
prune :: Int -> BTree a -> BTree a 
prune 0 _ = Empty
prune _ Empty = Empty
prune n (Node x e d) = (Node x (prune (n-1) e) (prune (n-1) d))

--1 e)
path :: [Bool] -> BTree a -> [a]
path l Empty = []
path [] (Node x e d) = [e]
path (h:t) (Node x e d)
    | h == True = x : path t d
    | otherwise = x : path t e

--1 f)
mirror :: BTree a -> BTree a 
mirror Empty = Empty
mirror (Node x e d) = (Node x (mirror d) (mirror e))

--1 g)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c 
zipWithBT f Empty _ = Empty
zipWithBT f _ Empty = Empty
zipWithBT f (Node x e d) (Node y l r ) = Node (f x y) (zipWithBT f e l) (zipWithBT f d r)

--2 a)
minimo :: Ord a => BTree a -> a 
minimo Empty = error "árvore vazia"
minimo (Node x Empty _) = x
minimo (Node x e d) = minimo e

--2 b)
semMinimo :: Ord a => BTree a -> BTree a 
semMinimo Empty = error "árvore vazia"
semMinimo (Node x Empty _) = Empty
semMinimo (Node x e d) = Node x (semMinimo e) d

--2 c)
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin Empty = error "árvore vazia"
minSmin (Node x Empty d) = (x, d)
minSmin (Node x e d) = (r_minimo, Node x r_semMinimo d)
    where (r_minimo, r_semMinimo) = minSmin e

--2 d)
remove :: Ord a => a -> BTree a -> BTree a
remove n Empty = error "elemento nao existe na arvore"
remove n (Node x e d)
    | n < x = (Node x (remove n e) d)
    | n > x = (Node x e (remove n d))
    | n == x = case d of Empty -> e
                         d -> let (r_minimo, r_semMinimo) = minSmin d in (Node r_minimo e r_semMinimo)

--3
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
               deriving Show
type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)

--3 a)
inscNum :: Numero -> Turma -> Bool
inscNum n Empty = False
inscNum n (Node (numero,_,_,_) e d) = n == numero = True || inscNome n (if n < numero then e else d)

--3 b)
inscNome :: Nome -> Turma -> Bool
inscNome n Empty = False
inscNome n (Node (_,nome,_,_) e d) = n == nome || inscNome n e || inscNome n d 

--3 c)
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (numero,nome,TE, _) e d) = trabEst e ++ [(numero,nome)] ++ trabEst d 
trabEst (Node _ e d) = trabEst e ++ trabEst d

--3 d)
nota :: Numero -> Turma -> Maybe Classificacao
nota n (Node (num,_,_,clas) e d) 
    | n == num = Just clas
    | n < num = nota n e 
    | otherwise = nota e d
nota _ _ = Nothing

--3 e)
percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas turma = sumfaltas turma / numeroalunos * 100
    where sumfaltas Empty = 0
          sumfaltas (Node (_,_,_,clas) e d) = (case clas of Faltou -> 1; otherwise = 0) + sumfaltas e + sumfaltas d
          numeroalunos Empty = 0
          numeroalunos (Node x e d) = 1 + numeroalunos e + numeroalunos d 

--3 f)
mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov

