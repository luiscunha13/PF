import Aula15nov
-- Uma árvore diz-se balanceada/equilibrada se é vazia, ou se verifica as seguintes condições
-- - as alturas das sub-árvores esquerda e direita diferem no máximo em uma unidade
-- - ambas as subárvores são balanceadasv

-- função que testa se uma arvore é balanceada
testbal :: BTree a -> Bool
testbal Empty = True
testbal (Node x e d) = abs ((altura e) - (altura d)) <= 1 && testbal e && testbal d 

-- função que serve para balancear uma árvored e procura
balancear :: BTree a -> BTree a
balancear t = constroi (inorder t)

constroi :: [a] -> BTree a 
constroi [] = Empty
constroi l = let n = length l 
                 (l1,x:l2) = splitAt (n `div` 2) l
             in Node x (constroi l1) (constroi l2)

--função melhorada da função balancear
balancear' :: BTree a -> BTree a 
balancear' t = constroi' (inorder t, length (inorder t))

constroi' :: ([a], Int) -> BTree a 
constroi' ([], 0) = Empty
constroi' (l,n) = let a = n `div` 2
                      (l1,x:l2) = splitAt a l
                  in Node x (constroi' (l1,2)) (constroi' (l2,n-a-1))

--função que faz a travessia da árvore por níveis
{-
niveis :: BTree a -> [a]
niveis t = aux [t]  

aux :: [BTree a] -> a 
aux [] = []
aux (Empty:t) = aux t 
aux ((Node x e d):t) = x : aux (t ++ [e,d]) -}

-- Árvores variáveis
-- Nas árvores irregulares cada nodo pode ter um número variável de descendentes. 
-- O seguinte tipo de dados é uma implementação de árvores irregulares, não vazias

data RTree a = R a [RTree a]
   deriving (Show)

-- função que conta os nodos de uma árvore variável
contaRT :: RTree a -> Int 
contaRT (R x l) = 1 + sum (map contaRT l)

--função que dá a altura de uma árvore variável
alturaRT :: RTree a -> Int
alturaRT (R x []) = 1
alturaRT (R x l) = 1 + maximum (map alturaRT l)

--travessia preorder: visitar a raiz, depois a(s) árvore(s) esquerda(s) e a seguir a(s) árvore(s) direita(s)
preorderRT :: RTree a -> [a]
preorderRT (R x l) = x : concat (map preorderRT l)

--função que faz a travessia da árvore irregular por níveis
niveisRT :: RTree a -> [a]
niveisRT t = aux' [t]

aux' :: [RTree a] -> [a]
aux' [] = []
aux' ((R x l):t) = x : aux' (t++l)

-- função que testa se um elemento pertence a uma árvore
{-elemRT :: Eq a => a -> RTree a -> Bool
elemRT x (R y l) = x == y || (map (elemRT x) l) -- mal feito -}

--Leaf Tree
-- Árvores binárias em que a informação está apenas  nas folhas da árvore, os nós intermédias não têm informação

data LTree a = Tip a 
             | Fork (LTree a) (LTree a)

-- função que dá uma lista das folhas de uma leaf tree
folhas :: LTree a -> [a]
folhas (Tip x) = [x]
folhas (Fork e d) = folhas e ++ folhas d

--função que dá uma lista de tuplos (folha, nível em que se encontra)
folhasNivel :: LTree a -> [(a,Int)]
folhasNivel (Tip x) = [(x,1)]
folhasNivel (Fork e d) = map (\(x,n) -> (x,n+1)) (folhasNivel e ++ folhasNivel d)

-- função que reconstroi uma leaf tree a partir de uma lista de tuplos (folha, nível em que se encontra)
reconstroi :: [(a,Int)] -> LTree a  
reconstroi l = fst (auxl 1 l)

auxl :: Int -> [(a,Int)] -> (LTree a, [(a,Int)])
auxl n ((a,x):t) 
    | n == x = (Tip a, t)
    | n < x = (Fork e d, l2)
    where (e,l1) = auxl (n+1) ((a,x):t)
          (d,l2) = auxl (n+1) l1