module Task6 where

{-
В этом файле приведён код, написанный (совместными усилиями) на лекции
Модифицируйте представленный тут парсер таким образом, чтобы он поддерживал унарные операции 
(отрицание и факториал), а также числа с плавающей точкой
-}

import Text.Parsec hiding(digit)
import Data.Functor

type Parser a = Parsec String () a

digit :: Parser Char
digit = oneOf ['0'..'9']

int :: Parser String
int = many1 digit

double :: Parser String
double = do
            char '.'
            res <- int
            return ('.':res)

number :: Parser Double
number = do
           num <- int
           opt <- option "" double
           return $ read (num ++ opt)

fact :: Double -> Double
fact n = factTail n 1

factTail :: Double -> Double -> Double
factTail 0 acc = acc
factTail 1 acc = acc
factTail n acc = factTail (n - 1) (acc * n)

applyMany :: a -> [a -> a] -> a
applyMany x [] = x
applyMany x (h:t) = applyMany (h x) t

div_ :: Parser (Double -> Double -> Double)
div_ = do
            char '/'
            return (/)

star :: Parser (Double -> Double -> Double)
star = do
            char '*'
            return (*)

plus :: Parser (Double -> Double -> Double)
plus = do
            char '+'
            return (+)

minus :: Parser (Double -> Double -> Double)
minus = do
            char '-'
            return (-)

negative :: Parser Double
negative = atom <|> do
                        char '-'
                        spaces
                        res <- atom
                        return $ negate res

factorial :: Parser (Double -> Double)
factorial = do
                char '!'
                return fact

factorization :: Parser Double
factorization = do
                    spaces
                    lhv <- atom
                    spaces
                    t <- many factorial
                    return $ applyMany lhv t

multiplication :: Parser Double
multiplication = do
                    spaces
                    lhv <- atom
                    spaces
                    t <- many tail
                    return $ applyMany lhv t
                    where tail = do
                                    f <- star <|> div_
                                    spaces
                                    rhv <- atom
                                    spaces
                                    return (`f` rhv)

addition :: Parser Double
addition = do
                spaces
                lhv <- negative <|> multiplication
                spaces
                t <- many tail
                return $ applyMany lhv t
                where tail = do
                                f <- plus <|> minus
                                spaces
                                rhv <- multiplication
                                spaces
                                return (`f` rhv)

atom :: Parser Double
atom = number <|> do
                    char '('
                    res <- addition
                    char ')'
                    return res

mainParser :: Parser Double
mainParser = do
                spaces
                p <- addition
                eof
                return p
