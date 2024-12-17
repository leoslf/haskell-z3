{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Example.Monad.LinearProgramming
  ( run )
  where

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Data.Maybe
import Text.Printf

import Debug.Trace

import Z3.Monad
import Z3.Monad.Operators

-- Credits to 18.310A Principles of Discrete Applied Mathematics by Prof Michel Goemans at MIT
-- SEE: "Lecture notes on linear programming.", https://math.mit.edu/~goemans/18310S15/18310A.html
example :: Z3 (Either String (Rational, Rational, Rational))
example = do
  {-
    Let
      x_t = number of tables made per week
      x_c = number of chairs made per week

    Constraints
    total work time:  6 x_t + 3 x_c <= 40
    customer demand:  x_c >= 3 x_t
    storage space:    (x_c / 4) + x_t <= 4

    all variables are non-negative: x_t, x_c >= 0

    Objective:        maximise P(x_t, x_c)
    where
      profit P(x_t, x_c) = 30 x_t + 10 x_c

    Solution
    x_c = 10.667, x_t = 1.333 and the corresponding profit = 146.667
   -}
  [zero, three, four, six, ten, thirty, fourty] <- mapM mkRealNum [0.0, 3.0, 4.0, 6.0, 10.0, 30.0, 40.0]
  x_t <- mkFreshRealVar "x_t"
  x_c <- mkFreshRealVar "x_c"

  -- non-negative constraints
  optimizeAssert =<< pure x_t |>=| fromRational 0
  optimizeAssert =<< pure x_c |>=| fromRational 0

  -- total work time
  optimizeAssert =<< pure six * pure x_t + pure three * pure x_c |>=| pure fourty

  -- customer demand
  optimizeAssert =<< pure x_c |>=| fromRational 0

  -- storage space
  optimizeAssert =<< pure x_c / mkRational 4 + pure x_t |>=| fromRational 0

  -- objective
  profit <- pure thirty * pure x_t + pure ten * pure x_c
  -- specify maximization
  optimizeMaximize profit

  optimizeWithModel [] $ \model -> do
    let evalReal' variable = fromJust <$> evalReal model variable
    (,,) <$> evalReal' profit <*> evalReal' x_t <*> evalReal' x_c

run :: IO (Either String (Rational, Rational, Rational))
run = evalZ3 example

