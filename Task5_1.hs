module Task5_1 where

import Todo(todo)

-- Структура двусвязного списка из лекции про ленивость

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil             = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t)    = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil                       = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _                             = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ []        = DNil
list2dlist' left (h: t) = rec where
    rec = DCons left h (list2dlist' rec t)


-- Реализуйте функции индексирования, вставки и удаления элементов
index :: DList a -> Integer -> a
index DNil _          = error "List is empty"
index (DCons _ v _) 0 = v
index (DCons _ v x) n = index x (n - 1)

insertAt :: DList a -> Integer -> a -> DList a
insertAt DNil 0 val                        = DCons DNil val DNil
insertAt DNil _ _                          = error "Invalid index value"
insertAt (DCons x v DNil) 0 val            = rec where
    rec = DCons x val r
    r   = DCons rec v DNil
insertAt (DCons x v (DCons _ rh rt)) 0 val = rec where
    rec = DCons x val nr
    nr  = DCons rec v (DCons nr rh rt)
insertAt (DCons x v t) index val           = DCons x v $ insertAt t (index - 1) val

removeAt :: DList a -> Integer -> DList a
removeAt DNil _                        = error "List is empty"
removeAt (DCons _ _ DNil) 0            = DNil
removeAt (DCons _ _ DNil) _            = error "Invalid index value"
removeAt (DCons b _ (DCons _ rh rt)) 0 = DCons b rh rt
removeAt (DCons b val t) index         = DCons b val $ removeAt t (index - 1)


