module Task4_1 where

{-
  Задание 4.1
  Реализация монады над функцией.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

--The Functor class describes parameterized types with data that can be converted.
instance Functor FunMonad where

--`fmap` apply function to packed value
    fmap f (FunMonad str) = FunMonad (f . str)

--The Applicative class describes parameterized types with defined operations which put values in type context and organize sequence of calculations.
instance Applicative FunMonad where
    pure = return
    f <*> x = f >>= \f' -> x >>= \x' -> return (f' x')

--The Monad class describes parameterized types for which operations are defined to organize a sequence of actions that can be described using the "do" syntax.
instance Monad FunMonad where

--`return` returns the given value indepedently from argument's type
    return x = FunMonad (\_ -> x)

--`>>=` operation apply function f to value in context of x, which returns the value in the context, a function is created in which the value is taken from the context (x $ y), and the specified function (f . fa $ y) is applied to it. Next, the function (fun . f . fa $ y) is taken from the resulting monad and the argument y is applied to it.
    (FunMonad x) >>= f = FunMonad (\y -> (fun . f . x $ y) $ y)
