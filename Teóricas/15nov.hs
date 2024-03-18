module Aula15nov where
--Tipos algébricos

-- Lista
-- data [a] = [] -- a lista é vazia
--          | (:) a [a] -- ou tem um elemento e uma sub estrutura que também é uma lista

--Árvore binária
-- ou é vazia
-- ou tem um elemento e duas sub-estruturas que também são árvores

--data BTree a = Empty
--            | Node a (BTree a) (BTree a)               Empty :: BTree a          Node :: a -> Btree l ->  Btree r -> BTree a 

--          A
--        /  \
--       B    C
--             \
--              D
-- Node A (Node B Empty Empty) (Node C Empty (Node D Empty Empty))

-- o nodo A é a raiz da árvore
-- os nodos B e C são filhos ( ou descendentes) de A
-- o nodo C é o pai de D
-- B e C são folhas da árvore
-- o caminho (path) de um nodo é a sequência de nodos da raiz até a esse nodo. Ex: A,C,D é o caminho para o nodo D
-- a altura da árvore é o comprimento do caminho mais longo. Esta árvore tem altura 3

data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving (Show)

--função que conta o número de nodos de uma lista
conta :: BTree a -> Int
conta Empty = 0
conta (Node x e d) = 1 + conta e + conta d

--função que soma todos os nodos de uma árvore de números
sumBT :: Num a => BTree a -> a 
sumBT Empty = 0
sumBT (Node x e d) = x + sumBT e + sumBT d 

--função que calcula a altura de uma lista
altura :: BTree a -> Int
altura Empty = 0
altura (Node _ e d) = 1 + max (altura e) (altura d)

--função map para árvores binárias
mapBT :: (a -> b) -> BTree a -> BTree b 
mapBT f Empty = Empty
mapBT f (Node x e d) = Node (f x) (mapBT f e) (mapBT f d)

--função zip para árvores binárias
zipBT :: BTree a -> BTree b -> BTree (a,b)
zipBT (Node x e d) (Node y e' d') = Node (x,y) (zipBT e e') (zipBT d d')
zipBT _ _ = Empty

--Travessias de árvores binárias

--travessia preorder: visitar a raiz, depois a árvore esquerda e a seguir a árvore direita
preorder ::BTree a -> [a]
preorder Empty = []
preorder (Node x e d) = [x] ++ (preorder e) ++ (preorder d)

--travessia inorder: visita a árvore esquerda, depois a raiz e a seguir a árvore direita
inorder :: BTree a -> [a]
inorder Empty = []  
inorder (Node x e d) = (inorder e) ++ [x] ++ (inorder d)

--travessia postorder: visitar a árvore esquerda, depois a árvore direita e a seguir a raiz
postorder :: BTree a -> [a]
postorder Empty = []
postorder (Node x e d) = (postorder e) ++ (postorder d) ++ [x]

--Árvores binárias de procura
-- Uma árvore binária em que o valor de cada nodo é maior do que os nodos à sua esquerda, e menor do que os nodos à sua direita diz-se uma árvore binária de procura (ou de pesquisa)
-- - a raiz da árvore é maior do que todos os elementos que estão na sub-árvore esquerda.
-- - ...
-- - ...

--função que testa se um elemento eprtence a uma árvore binária de procura
elemBT :: Ord a => a -> BTree a -> Bool
elemBT y Empty = False
elemBT y (Node x e d)
    | y == x = True
    | y < x = elemBT y e
    | otherwise = elemBT y d 

--função que insere um elemento numa árvore binária de procura
insereBT :: Ord a => a -> BTree a -> BTree a 
insereBT a Empty = Node a Empty Empty
insereBT a (Node x e d)
    | a == x = Node x e d 
    | a < x = Node x (insereBT a e) d 
    | otherwise = Node x e (insereBT a d)

-- o formato de uma árvore depende da ordem pela qual os elementos vão sendo inseridos 

--função que recebe ua lista e converte numa árvore binária de procura
listToBT :: Ord a => [a] -> BTree a 
listToBT [] = Empty
listToBT (h:t) = insereBT h (listToBT t)

listToBT' l = foldr insereBT Empty l --com funções de ordem superior


listToBT'' l = foldl (\ac x -> insereBT x ac) Empty l