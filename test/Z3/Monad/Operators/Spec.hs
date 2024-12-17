module Z3.Monad.Operators.Spec
  ( spec )
  where

import Test.Hspec

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.Maybe

import Z3.Monad
import qualified Z3.Monad as Z3
import Z3.Monad.Operators

spec :: Spec
spec = do
  context "operators" $ do
    let verbose :: Z3 (Either String (Rational, Rational, Rational))
        verbose = do
          [zero, three, four, six, ten, thirty, fourty] <- mapM mkRational [0.0, 3.0, 4.0, 6.0, 10.0, 30.0, 40.0]
          x_t <- mkFreshRealVar "x_t"
          x_c <- mkFreshRealVar "x_c"

          -- non-negative constraints
          optimizeAssert =<< mkGe x_t zero
          optimizeAssert =<< mkGe x_c zero

          -- total work time
          optimizeAssert =<< (`mkLe` fourty) =<< mkAdd =<< sequence [mkMul [six, x_t], mkMul [three, x_c]]

          -- customer demand
          optimizeAssert =<< mkGe x_c =<< mkMul [three, x_t]

          -- storage space
          optimizeAssert =<< mkGe four =<< mkAdd =<< (:[x_t]) <$> mkDiv x_c four

          -- objective
          profit <- mkAdd =<< sequence [mkMul [thirty, x_t], mkMul [ten, x_c]]
          -- specify maximization
          optimizeMaximize profit

          optimizeWithModel [] $ \model -> do
            let eval variable = fromJust <$> evalReal model variable
            (,,) <$> eval profit <*> eval x_t <*> eval x_c

        precise :: Z3 (Either String (Rational, Rational, Rational))
        precise = do
          x_t <- mkFreshRealVar "x_t"
          x_c <- mkFreshRealVar "x_c"

          -- non-negative constraints
          optimizeAssert =<< pure x_t |>=| fromRational 0
          optimizeAssert =<< pure x_c |>=| fromRational 0

          -- total work time
          optimizeAssert =<< mkRational 6 * pure x_t + mkRational 3 * pure x_c |<=| mkRational 40
          -- customer demand
          optimizeAssert =<< pure x_c |>=| mkRational 3 * pure x_c
          -- storage space
          optimizeAssert =<< pure x_c / mkRational 4 + pure x_t |>=| fromRational 0

          -- objective
          profit <- fromRational 30 * pure x_t + fromRational 10 * pure x_c
          -- specify maximization
          optimizeMaximize profit

          optimizeWithModel [] $ \model -> do
            let eval variable = fromJust <$> evalReal model variable
            (,,) <$> eval profit <*> eval x_t <*> eval x_c

    it "should be usable in monadic context and produce the same results" $ do
      precise' <- liftIO $ Z3.evalZ3 precise
      verbose' <- liftIO $ Z3.evalZ3 verbose
      precise' `shouldBe` verbose'




