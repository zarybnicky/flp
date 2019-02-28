module Lib where

import Data.List


data Vector a = Vector Int [a]

initVector :: [a] -> Vector a
initVector xs = Vector (length xs) xs

dotProd :: Num a => Vector a -> Vector a -> Maybe a
dotProd (Vector m xs) (Vector n ys) =
  if m == n
  then Just (sum $ zipWith (*) xs ys)
  else Nothing


data Teleso
  = Kvadr { kvadrA :: Double, kvadrB :: Double, kvadrC :: Double }
  | Kuzel { kuzelA :: Double, kuzelB :: Double }
  | Koule { kouleA :: Double }


data LExp
  = LVar String
  | LApp LExp LExp
  | LAbs String LExp
  deriving Show

freeVars :: LExp -> [String]
freeVars = go []
  where
    go vs (LVar v) = v:vs
    go vs (LApp e1 e2) = go vs e1 ++ go vs e2
    go vs (LAbs v e) = filter (/= v) (go vs e)

subst :: String -> LExp -> LExp -> LExp
subst x ex (LVar y)
  | x == y = ex
  | otherwise = LVar y
subst x ex (LApp e1 e2) = LApp (subst x ex e1) (subst x ex e2)
subst x ex (LAbs y e)
  | x == y = subst x ex e
  | otherwise = LAbs y (subst x ex e)
-- reduce1 :: one step of reduction
-- reduceNF :: fix-point


