module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f x [] = x
foldl f x (y:ys) = foldl f (f x y) ys 

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x [] = x
foldr f x (y:ys) = f y (foldr f x ys)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f x = case f x of
                 Just (x', y) -> x' : unfoldr f y
                 Nothing -> []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x acc -> (f x) : acc) []

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product lst = foldr (*) 1 lst

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (maybe id (:)) []

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal mtr = snd $ foldr (\x (index, lst) -> (index - 1, x !! index : lst)) (length mtr - 1, []) mtr

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = foldr (\x s -> if f x then s else (x:s)) []

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem w = foldr (\x y -> if (x == w) then True else y) False

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr (\x -> if (x >= to) then Nothing else Just (x, x + step)) from

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append a b = foldr (:) b a

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = unfoldr (\x -> if null x then Nothing else Just (split n x)) lst

split :: Integer -> [a] -> ([a], [a])
split n = foldr (\x (xs, xz) -> if fst x <= n then (snd x : xs, xz) else (xs, snd x : xz)) ([],[]) . zip[1..]
