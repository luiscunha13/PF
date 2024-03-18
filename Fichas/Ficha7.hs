data RTree a = R a [RTree a] -- Rose Tree

arvore_r = R 5 [R 2 [R 1 [], 
                     R 0 [], 
                     R 3 []], 
                R 7 [], 
                R 9 [R 15 []], 
                R 4 [R 6 [], 
                     R 0 []]]

data ExpInt = Const Int
| Simetrico ExpInt
| Mais ExpInt ExpInt
| Menos ExpInt ExpInt
| Mult ExpInt ExpInt

--1 a)
calcula :: ExpInt -> Int
calcula (Cons a) = a
calcula (Simetrico a) = - (calcula a)
calcula (Mais a b) = calcula a + calcula b
calcula (Menos a b) = calcula a - calcula b 
calcula (Mult a b) = calcula a * calcula b

--1 b)




--2 a)
soma :: Num a => RTree a -> a
soma (R x []) = x
soma (R x l) = e + sum (map (soma) l)

--2 b)
altura :: RTree a -> Int
altura (R x []) = 1
altura (R x filhos) = 1 + maximum (map altura filhos)

--2 c)
prune :: Int -> RTree a -> RTree a
prune 0 (R x l) = R e []
prune n (R x l) = R x (map (prune (n-1)) l)

--2 d)
mirror :: RTree a -> RTree a 
mirror (R x []) = R x []
mirror (R x l) = R x (map mirror (reverse l))

-- 2 e)
postorder :: RTree a -> [a]
postorder (R a []) = [a]
postorder (R a filhos) = concat (map postorder filhos) ++ [a]

-- fazer em casa as alíneas c) e d)

data LTree a = Tip a | Fork (LTree a) (LTree a) -- Leaf Tree

arvore_l = Fork (Fork (Fork (Tip 3) (Tip 5)) (Fork (Tip 1) (Tip 8))) (Fork (Fork (Tip 7) (Tip 9)) (Fork (Tip 4) (Tip 6)))
arvore_b = Node 5 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 8 (Node 7 Empty Empty) (Node 9 Empty Empty))
--3 a)
ltSum :: Num a => LTree a -> a  
ltSum (Tip x) = x
ltSum (Fork e d) = ltSum e + ltSum d

--3 b)
listaLT :: Ltree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork e d) = listaLT e ++ listaLT d

--3 c)
ltHeight :: LTree a -> Int
ltHeight (Tip x) = 1
ltHeight (Fork e d) = 1 + max (ltHeight e) (ltHeight d)


data BTree a = Empty | Node a (BTree a) (BTree a)
data FTree a b = Leaf b | No a (FTree a b) (FTree a b) -- não existe empty em full trees (tudo tem informação)
          deriving Show

-- 4 a)
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf x) = (Empty, Tip x)
splitFTree (No x e d) = (Node x btreel btreer, Fork ltreel ltreer)
    where (btreel, ltreel) = splitFTree e 
          (btreer, ltreer) = splitFTree d

-- 4 b)
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Fork _ _) = Nothing
joinTrees (Node _ _ _) (Tip _) = Nothing
joinTrees Empty (Tip x) = Just (Leaf x)
joinTrees (Node x e d) (Fork l r) = case (joinTrees e l, joinTrees d r) of (Nothing, _) -> Nothing 
                                                                           (_, Nothing) -> Nothing
                                                                           (Just full_l, Just full_r) -> Just (No x full_l full_r) 
    where full_l = joinTrees e l 
          full_r = joinTrees e r



