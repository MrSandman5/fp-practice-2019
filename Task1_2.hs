module Task1_2 where

import Todo(todo)

fact :: Integer -> Integer
fact x | x == 0 = 1
       | x > 0 = x * fact (x - 1)
       | otherwise = error "arg must be >=0"

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
gcd' :: Integer -> Integer -> Integer
gcd' x 0 = x
gcd' x y = gcd' b (mod a b)
    where a = abs x
          b = abs y 

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist 0 0 = False
doesSquareBetweenExist x 0 = False
doesSquareBetweenExist x y | x == y = False
                           | sq * sq == x = True 
                           | otherwise    = doesSquareBetweenExist (x+1) y
                           where sq = floor $ sqrt $ (fromIntegral x :: Double)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = todo

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow 0 0 = 0
pow 0 y = 0
pow x 0 = 1
pow x y = x * pow x (y-1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n | n == 1 = False
          | n == 2 = True
          | (length [x | x <- [2..n-1], mod n x == 0]) > 0 = False
          | otherwise = True

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
