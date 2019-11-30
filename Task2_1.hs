module Task2_1 where

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}

import Todo(todo)

import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = EmptyTree
               | Leaf (Integer, v)
               | Node (Integer, v) (TreeMap v) (TreeMap v)
               deriving (Show, Eq, Read)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTree _ = False
contains (Leaf (key, value)) leaf | leaf==key = True
                                  | otherwise = False
contains (Node (key, value) left right) node | node == key = True
                                             | node < key = contains left node
                                             | node > key = contains right node
                                             | otherwise = False 

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup _ EmptyTree = error "tree is empty, so there is no keys and values"
lookup key (Leaf (k, v)) | key == k = v
                         | otherwise = error "there is no such key in the tree"
lookup key (Node (k, v) left right) | key == k = v
                                    | key < k = lookup key left
                                    | key > k = lookup key right
                                    | otherwise = error "there is no value for the key" 

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (key, value) EmptyTree = Leaf (key, value)
insert (key, value) (Leaf (k, v)) | key == k = Leaf (key, value)
                                  | key < k = Node (k, v) (Leaf (key, value)) EmptyTree
                                  | key > k = Node (k, v) EmptyTree (Leaf (key, value))
                                  | otherwise = error "there is no such key in the tree" 
insert (key, value) (Node (k, v) left right) | key == k = Node (key, value) left right
                                             | key < k = Node (k, v) (insert (key, value) left) right
                                             | key > k = Node (k, v) left (insert (key, value) right)
                                             | otherwise = error "there is no such key in the tree" 


-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove _ EmptyTree = error "tree is empty, so there is no keys and values"
remove key (Leaf (k, v)) | key == k = EmptyTree
                         | otherwise = error "there is no such key in the tree"
remove key (Node (k, v) left right) | key == k = replace left right
                                    | key < k = Node (k, v) left (remove key right)
                                    | key > k = Node (k, v) (remove key left) right
                                    | otherwise = error "there is no such key in the tree"
        where
            replace :: TreeMap v -> TreeMap v -> TreeMap v
            replace EmptyTree tree = tree
            replace (Leaf (k, v)) tree = Node (k, v) EmptyTree tree
            replace (Node (k, v) l r) tree = Node (k, v) l (replace r tree)

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE _ EmptyTree = error "tree is empty, so there is no keys and values"
nearestLE key (Leaf (k, v)) | key == k = (k, v)
                            | otherwise = error "there is no nearest element"
nearestLE key (Node (k, v) left right) | key == k = (k, v)
                                       | key < 0 && isEmpty left = error "there is no nearest element"
                                       | key > 0 && isEmpty right = (k, v)
                                       | key < 0 && isLeaf left = if (key >= (fst $ getPair left)) 
                                         then getPair left 
                                         else error "there is no nearest element"
                                       | key < 0 && isNode left = if (key >= (fst $ getPair left))
                                         then
                                             if (isNode $ getRight left)
                                             then nearestLE key (getRight left)
                                             else
                                                 if (isEmpty $ getRight left) then getPair left
                                                 else
                                                     if ((fst $ getPair $ getRight left) > key) then getPair left
                                                     else nearestLE key (getRight left)
                                         else nearestLE key left
                                       | otherwise = if (getLeftKey right) <= key then nearestLE key right else (k, v)

isEmpty :: TreeMap v -> Bool
isEmpty EmptyTree = True
isEmpty _ = False

isLeaf :: TreeMap v -> Bool
isLeaf (Leaf (key, value)) = True
isLeaf _ = False

isNode :: TreeMap v -> Bool
isNode (Node (key, value) left right) = True
isNode _ = False

getRight :: TreeMap v -> TreeMap v
getRight (Node (key, value) left right) = right

getPair :: TreeMap v -> (Integer, v)
getPair (Leaf (key, value)) = (key, value)
getPair (Node (key, value) left right) = (key, value)

getLeftKey :: TreeMap v -> Integer
getLeftKey (Leaf (key, value)) = key
getLeftKey (Node (key, value) EmptyTree _) = key
getLeftKey (Node (key, value) l _) = getLeftKey l


-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList [] = EmptyTree
treeFromList (x:xs) = insert x (treeFromList xs)

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmptyTree = []
listFromTree (Leaf (k, v)) = [(k, v)]
listFromTree (Node (k, v) l r) = [(k, v)] ++ listFromTree l ++ listFromTree r 

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean _ EmptyTree = error "tree is empty, so there is no keys and values"
kMean n (Leaf (k, v)) | n == 0 = (k, v)
                      | otherwise = error "there is no such index"
kMean n (Node (k, v) EmptyTree (Leaf (k1, v1))) | n == 0 = (k, v)
                                                | n == 1 = (k1, v1)
                                                | otherwise = error "there is no such index"
kMean n (Node (k, v) (Leaf (k1, v1)) EmptyTree) | n == 0 = (k, v)
                                                | n == 1 = (k1, v1)
                                                | otherwise = error "there is no such index"
kMean n (Node (k, v) (Leaf (k1, v1)) (Leaf (k2, v2))) | n == 0 = (k, v)
                                                      | n == 1 = (k1, v1)
                                                      | n == 2 = (k2, v2)
                                                      | otherwise = error "there is no such index"
kMean n (Node (k, v) l r) | sizeOf l == n = (k, v)
                          | sizeOf l >= (n - 1) = kMean n l
                          | otherwise = kMean (n - (sizeOf l + 1)) l

sizeOf :: TreeMap v -> Integer
sizeOf EmptyTree = (-1)
sizeOf (Leaf (k, v)) = 1
sizeof (Node (k, v) l r) = sizeOf l + sizeOf r + 1
