--quick sort

qSort ::[Int] -> [Int]
qSort [] = []
qSort (h:t) = (qSort l1)++[h]++(qSort l2) 
    where (l1,l2) = parte h t 

parte :: Int -> [Int] -> ([Int],[Int])
parte x [] = ([],[])
parte x (h:t) = if h<x 
                then (h:a,b) 
                else (a,h:b)
    where (a,b) = parte x t

-- merge sort

mSort :: [Int] -> [Int]
mSort [] = []
mSort [x] = [x]
mSort l = merge (mSort l1) (mSort l2)
    where (l1,l2) = splitAt ((length l) `div` 2) l 

merge :: [Int] -> [Int] -> [Int]
merge [] l = l
merge l [] = l
merge (h:t) (h1:t1)
            |h<h1 = h : merge t (h1:t1)
            |otherwise = h1 : merge (h:t) t1

--funções com parametro de acumulação

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (h:t) = (reverse1 t) ++ [h]-- sem acumulação

reverse' :: [a] -> [a]
reverse' l = inverteAc l [] -- [] é o acumulador

inverteAc :: [a] -> [a] -> [a]
inverteAc (h:t) ac = inverteAc t (h:ac)
inverteAc [] ac = ac -- com acumulação

sum1 :: Num a => [a] -> a
sum1 [] = 0 
sum1 (h:t) = h+sum1 t -- sem acumulador

--sum' :: Num a => [a] -> [a]
--sum' l = sumAc l 0
--    where sumAc (h:t) ac = sumAc t (h+ac)
--          sumAc [] ac = ac  está mal ver depois

maximo :: Ord a => [a] -> a 
maximo (h:t) = maximoAc t h

maximoAc :: Ord a => [a] -> a -> a 
--maximoAc (h:t) ac = if x> ac then maximoAc t h else maximoAc t ac
maximoAc [] ac = ac 
maximoAc (h:t) ac = maximoAc t (max h ac)

fact :: Integer -> Integer
fact n = factAc n  1 
    where factAc n ac | n > 0 = factAc (n-1) (n*ac)
          fact 0 ac  =ac




