module Task1_1 where

import Todo(todo)

data BinaryOperation = Plus | Minus | Mult
    deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, binOp :: BinaryOperation, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
infixr 6 |+|
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm l Plus r

infixr 6 |-|
(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm l Minus r

infixr 7 |*|
(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm l Mult r

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of
    (Variable var)      -> if (var == varName) then replacement else expression
    (BinaryTerm l op r) -> BinaryTerm (replaceVar varName replacement l) op (replaceVar varName replacement r)
    _                   -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = case expression of
    BinaryTerm l op r ->
        case (left, op, right) of
            (IntConstant 0, Plus,                _)  -> right
            (            _, Plus,    IntConstant 0)  -> left
            (IntConstant c1, Plus,  IntConstant c2)  -> IntConstant $ c1+c2
            (            _, Minus,   IntConstant 0)  -> left
            (IntConstant c1, Minus, IntConstant c2)  -> IntConstant $ c1-c2
            (IntConstant 0, Mult,                _)  -> IntConstant 0
            (            _, Mult,    IntConstant 0)  -> IntConstant 0
            (IntConstant 1, Mult,                _)  -> right
            (            _, Mult,    IntConstant 1)  -> left
            (IntConstant c1, Mult,  IntConstant c2)  -> IntConstant $ c1*c2
            _                                        -> BinaryTerm left op right
        where
            left = evaluate l
            right = evaluate r
    _                 -> expression
