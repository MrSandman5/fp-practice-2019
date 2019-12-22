module Task3_3 where

import Todo(todo)

{-
  Задание 3.3
  Множество на основе предикатов
-}

newtype PSet a = PSet{ contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

-- List of operations that must be applied to set:
-- 	1) union
--	2) intersept
--	3) symmetric difference
-- Difference and cartesian product can't be applied because they don't obey associativity and identity laws
-- In that case, we create three new types for set operations

-- Monoids have to follow monoid's laws:
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z) -- associativity
-- mempty `mappend` x = x               -- left identity
-- x `mappend` mempty = x               -- right identity

newtype UnionPSet a = UnionPSet{ uContains :: (a -> Bool) }

instance Monoid (UnionPSet a) where
    mempty = UnionPSet (\_ -> False)
    mappend (UnionPSet x) (UnionPSet y)= UnionPSet (\z -> x z || y z)

newtype IntersectPSet a = IntersectPSet{ iContains :: (a -> Bool) }

instance Monoid (IntersectPSet a) where
    mempty = IntersectPSet (\_ -> True)
    mappend (IntersectPSet x) (IntersectPSet y) = IntersectPSet (\z -> x z && y z)

newtype SubPSet a = SubPSet{ sContains :: (a -> Bool) }

instance Monoid (SubPSet a) where
    mempty = SubPSet (\_ -> False)
    mappend (SubPSet x) (SubPSet y) = SubPSet (\z -> (x z && y z) && not (x z && y z))

-- To create Functor current type, PSet must be covariant with argument
-- As an example: fmap :: Functor f => (a -> b) -> f a -> f b
-- But with 'a -> Bool' argument we have got something like this:
-- fmap :: Functor PSet => ((a -> Bool) -> (b -> Bool)) -> PSet (a -> Bool) -> PSet (b -> Bool)
-- Argument of PSet is polymorpic so we can't apply function to an argument of function

