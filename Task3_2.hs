module Task3_2 where

{-
  Задание 3.2
  Структура данных "перевёрнутый список" представляет собой зеркально обращённый
  односвязный список из стандартной библиотеки, для которого взятие последнего
  элемента является наиболее эффективной операцией.
  Необходимо реализовать все функции и классы типов в данном файле.
-}

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList RNil = []
rlistToList (RCons xs x) = (rlistToList xs) ++ [x]

listToRList :: [a] -> ReverseList a
listToRList [] = RNil
listToRList x = RCons (listToRList $ init x) (last x)

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Eq a) => Eq (ReverseList a) where
    (==) RNil RNil = True
    (==) RNil _    = False
    (==) _ RNil    = False
    (==) (RCons xs x) (RCons ys y) | x == y    = xs == ys
                                   | otherwise = False                                    

instance (Ord a) => Ord (ReverseList a) where
    RNil `compare` RNil = EQ
    RNil `compare` (RCons xs x) = LT
    (RCons xs x) `compare` RNil = GT
    (RCons xs x) `compare` (RCons ys y) | x > y     = GT
                                        | x < y     = LT
                                        | otherwise = xs `compare` ys

instance (Show a) => Show (ReverseList a) where
    show RNil = "RNil"
    show (RCons xs x) = "(RCons " ++ show xs ++ " " ++ show x ++ ")"

instance Monoid (ReverseList a) where
    mempty = RNil
    mappend x RNil = x
    mappend RNil x = x
    mappend x (RCons ys y) = RCons (mappend x ys) y

instance Functor ReverseList where
    fmap f RNil = RNil
    fmap f (RCons xs x) = RCons (f <$> xs) (f x)
