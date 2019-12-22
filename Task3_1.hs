module Task3_1 where

{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

toInt :: WeirdPeanoNumber -> Int
toInt x = toInt' x 0 where
    toInt' Zero y     = y
    toInt' (Succ x) y = toInt' x (y+1)
    toInt' (Pred x) y = toInt' x (y-1)

fromInt :: Int -> WeirdPeanoNumber
fromInt x | x > 0     = Succ (fromInt (x-1))
          | x < 0     = Pred (fromInt (x+1))
          | otherwise = Zero

norm :: WeirdPeanoNumber -> WeirdPeanoNumber
norm = fromInt . toInt

-- Реализуйте все классы типов, которым должны отвечать целые числа

instance Show WeirdPeanoNumber where
    show Zero     = "0"
    show (Succ x) = "Succ(" ++ show x ++ ")"
    show (Pred x) = "Pred(" ++ show x ++ ")"

instance Enum WeirdPeanoNumber where 
    toEnum x   = fromInt x
    fromEnum x = toInt x

instance Eq WeirdPeanoNumber where
    (==) a b = toInt a == toInt b

instance Ord WeirdPeanoNumber where
    a `compare` b = case (norm a, norm b) of
        (Zero, Zero)     -> EQ
        (Zero, Succ x)   -> LT
        (Zero, Pred x)   -> GT
        (Succ x, Succ y) -> x `compare` y
        (Succ x, _)      -> GT
        (Pred x, Pred y) -> x `compare` y
        (Pred x, _)      -> LT

instance Bounded WeirdPeanoNumber where
    minBound = fromInt minBound
    maxBound = fromInt maxBound

instance Real WeirdPeanoNumber where
    toRational x = toRational $ toInt x

instance Num WeirdPeanoNumber where
    (+) a Zero     = a
    (+) Zero a     = a
    (+) (Succ a) b = Succ (a+b)
    (+) (Pred a) b = Pred (a+b)

    (*) a b = if (signum a == signum b) 
                 then mult (f a) (f b)
                 else negate $ mult (f a) (f b)
              where
                 f = abs . norm
                 mult a Zero = Zero
                 mult Zero a = Zero
                 mult (Succ a) b = a * b + b

    abs a | a > Zero  = a
          | a < Zero  = negate a
          | otherwise = Zero

    signum a | a > Zero  = 1
             | a < Zero  = -1
             | otherwise = 0

    negate Zero     = Zero
    negate (Succ a) = Succ (negate a)
    negate (Pred a) = Pred (negate a)

    fromInteger a | a > 0     = Succ (fromInteger (a-1))
                  | a < 0     = Pred (fromInteger (a+1))
                  | otherwise = Zero

instance Integral WeirdPeanoNumber where
    toInteger Zero = 0
    toInteger (Succ a) = toInteger (a+1)
    toInteger (Pred a) = toInteger (a-1)

    quotRem a b = let (q, r) = quotRem' (f a) (f b)
                      m      = signum a
                  in if (signum a == signum b) 
                     then (q, m * r)
                     else (negate q, m * r)
                  where
                     f = abs . norm
                     quotRem' a b = case (a `compare` b) of
                         EQ | b == Zero -> error "Division by zero"
                         EQ | a == Zero -> (Zero, Zero)
                         EQ             -> (Succ Zero, Zero)
                         LT             -> (Zero, a)
                         GT             -> let (x1, x2) = quotRem' (f (a-b)) b
                                           in (x1 + (Succ Zero), Zero + x2)

