module Task5_2 where

import Todo(todo)

-- Зиппер из лекции 

data Zipper a = Zipper [a] [a]

-- Реализуйте экземпляры классов Show и Eq для этого типа

fromList :: [a] -> Zipper a
fromList lst = Zipper [] lst

toList :: Zipper a -> [a]
toList (Zipper l r) = concat [l, r]

instance (Show a) => Show (Zipper a) where
    show (Zipper l r) = show (reverse l) ++ show r  

instance (Eq a) => Eq (Zipper a) where
    (==) z1 z2 = toList z1 == toList z2 

goRight :: Zipper a -> Zipper a
goRight z@(Zipper _ []) = z
goRight (Zipper l (rh:rt)) = Zipper (rh:l) rt

goLeft :: Zipper a -> Zipper a
goLeft z@(Zipper [] _) = z
goLeft (Zipper (lh:lt) r) = Zipper lt (lh:r)

putRight :: a -> Zipper a -> Zipper a
putRight x (Zipper l r) = Zipper l (x:r)

putLeft :: a -> Zipper a -> Zipper a
putLeft x (Zipper l r) = Zipper (x:l) r

removeRight :: Zipper a -> Zipper a
removeRight (Zipper l (_:rt)) = Zipper l rt

removeLeft :: Zipper a -> Zipper a
removeLeft (Zipper (_:lt) r) = Zipper lt r

-- Используя приведённые выше функции, реализуйте функцию конкатенации
-- вставки подсписка в середину и выделения подсписка

concatZipper :: Zipper a -> Zipper a -> Zipper a
concatZipper left right = Zipper (toList left) (toList right)

insertManyAt :: Int -> Zipper a -> Zipper a -> Zipper a
insertManyAt index (Zipper [] []) to   = to
insertManyAt index from (Zipper [] []) = from
insertManyAt index from (Zipper li ri) | index < 0 || index > (length $ concat [li,ri]) = error "Incorrect Index" 
                                       | otherwise = insertManyAt' index from (Zipper [] (concat [li,ri]))

insertManyAt' :: Int -> Zipper a -> Zipper a -> Zipper a
insertManyAt' 0 (Zipper lw rw) (Zipper li ri) = Zipper (concat [li,lw]) (concat [rw,ri])
insertManyAt' i sender into                   = insertManyAt' (i - 1) sender (goRight into)

subZipper :: Int -> Int -> Zipper a -> Zipper a
subZipper from to (Zipper [] []) = error "Zipper is empty"
subZipper from to (Zipper li ri) | from < 0 || from > (length $ concat [li,ri]) = error "Incorrect From"
                                 | to < 0 || to > (length $ concat [li,ri]) = error "Incorrect To"
                                 | from > to = error "Error from > to"
                                 | otherwise = Zipper [] $take (to - from + 1) $drop from $concat [li,ri]
