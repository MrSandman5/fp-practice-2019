module Task4_2 where

{-
  Задание 4.1
  Реализация монады над множеством из четырёх элементов.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

--The Functor class describes parameterized types with data that can be converted.
instance Functor FourOf where

--`fmap` apply function to all 4 elements
    fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)

--The Applicative class describes parameterized types with defined operations which put values in type context and organize sequence of calculations.
instance Applicative FourOf where

--`pure` operation set value x in context on all 4 places in set
    pure x = FourOf x x x x

--`<*>` operation extract elements (a2 b2 c2 d2) from context and applies functions (a1 b1 c1 d1) in context to each element
    (FourOf a1 b1 c1 d1) <*> (FourOf a2 b2 c2 d2) = 
        FourOf (a1 a2) (b1 b2) (c1 c2) (d1 d2)

--The Monad class describes parameterized types for which operations are defined to organize a sequence of actions that can be described using the "do" syntax.
instance Monad FourOf where
--`>>=` extract values (a b c d) from context and applies function f, which returns values in context, to each value. Then the values (fa fb fc fd) are extracted from the received four monads and placed in the context of the returned monad. The positions of the corresponding elements in the input set, in the output set, and in the set obtained as a result of applying the function f coincide. This allows you to implement the behavior described in the task.
    (FourOf a b c d) >>= f = FourOf fa fb fc fd where
        (FourOf fa _ _ _) = f a
        (FourOf _ fb _ _) = f b
        (FourOf _ _ fc _) = f c
        (FourOf _ _ _ fd) = f d
