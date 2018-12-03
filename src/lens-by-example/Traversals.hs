module Traversals where

import Control.Lens
import qualified Data.Map as M

data Transaction
  = Withdrawal { amount :: Int }
  | Deposit { amount :: Int }
  deriving (Show)

someTransactions :: [Transaction]
someTransactions = [Deposit 100, Withdrawal 50]

-- Traversal' s a 
-- s: Structure
-- a: focus
-- e.g.: Traversal' [a] a
-- `s` should wrap `a` type always
simpleTransactions :: Traversal' [Transaction] Transaction
simpleTransactions = traverse

setAll = someTransactions & simpleTransactions .~ (Deposit 75)

-- Traversal s e a b
-- s: Initial structure
-- e: Ending structure
-- a: initial foucs
-- b: end foucs
-- e.g:  Traversal [a] [b] a b
-- where r stands by result	 
typeChangingTransactions :: Traversal [Transaction] [r] Transaction r
typeChangingTransactions = traverse

-- All transactions traverse
transactionsToValue ::
     (Applicative f) => (Int -> f Int) -> [Transaction] -> f [Transaction]
transactionsToValue f ts = traverse pm ts
  where
    pm (Deposit v) = Deposit <$> f v
    pm (Withdrawal v) = Withdrawal <$> f v

-- Deposits *2
doubleDeposits ::
     (Applicative f) => (Int -> f Int) -> [Transaction] -> f [Transaction]
doubleDeposits f ts = traverse act ts
  where
    act (Deposit v) = Deposit <$> f (v * 2)
    act (Withdrawal v) = Withdrawal <$> pure v

--Selective traversals
--withdrawals 
applicativeTest :: Int -> [Transaction]
applicativeTest a = Deposit <$> [a]
