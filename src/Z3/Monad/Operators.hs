{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module    : Z3.Monad.Operators
--
module Z3.Monad.Operators where

import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (join, liftM2, (<=<))

import Z3.Monad

instance Real (Z3 AST) where
  toRational r = unsafePerformIO $ evalZ3 $ getReal =<< r

instance Eq (Z3 AST) where
  a == b = unsafePerformIO $ evalZ3 $ getBool =<< a |==| b

instance Ord (Z3 AST) where
  compare = undefined

instance Enum (Z3 AST) where
  toEnum = mkInteger . fromIntegral
  fromEnum n = fromIntegral $ unsafePerformIO $ evalZ3 $ getInt =<< n

instance Fractional (Z3 AST) where
  fromRational = mkRational
  a / b = join $ liftM2 mkDiv a b


instance Integral (Z3 AST) where
  a `div` b = join $ liftM2 mkDiv a b
  a `mod` b = join $ liftM2 mkMod a b
  a `divMod` b = (a `div` b, a `mod` b)

  a `quot` b = join $ liftM2 mkBvudiv a b
  a `rem` b = join $ liftM2 mkRem a b
  a `quotRem` b = (a `div` b, a `rem` b)

instance Num (Z3 AST) where
  a + b = mkAdd =<< sequence [a, b]
  a - b = mkSub =<< sequence [a, b]
  a * b = mkMul =<< sequence [a, b]
  abs n = do
    negative <- n |<| fromIntegral 0 >>= getBool
    if negative then negate n else n
  negate n = mkUnaryMinus =<< n
  signum n = do
    isInt <- getBool =<< mkIsInt =<< n
    let div' = if isInt then div else (/)
    negate n `div'` abs n
  fromInteger = mkInteger

(|==|) :: Z3 AST -> Z3 AST -> Z3 AST
a |==| b = join $ liftM2 mkEq a b

(|/=|) :: Z3 AST -> Z3 AST -> Z3 AST
a |/=| b = mkNot =<< a |==| b

(|<|) :: Z3 AST -> Z3 AST -> Z3 AST
a |<| b = join $ liftM2 mkLt a b

(|<=|) :: Z3 AST -> Z3 AST -> Z3 AST
a |<=| b = join $ liftM2 mkLe a b

(|>|) :: Z3 AST -> Z3 AST -> Z3 AST
a |>| b = join $ liftM2 mkGt a b

(|>=|) :: Z3 AST -> Z3 AST -> Z3 AST
a |>=| b = join $ liftM2 mkGe a b

(<&&>) :: Z3 AST -> Z3 AST -> Z3 AST
a <&&> b = mkAnd =<< sequence [a, b]

(<||>) :: Z3 AST -> Z3 AST -> Z3 AST
a <||> b = mkOr =<< sequence [a, b]

infix 4 |==|, |/=|, |<|, |<=|, |>=|, |>|
infix 3 <&&>
infix 2 <||>
