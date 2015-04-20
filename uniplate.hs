{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
import Data.Generics.Uniplate.Data

import Data.Data
import Data.List
import Data.Typeable
import GHC.Generics (Generic)

data Tree = Tree {
  text :: String,
  subtrees :: [Tree]
  } deriving (Show,Eq,Read,Generic, Data, Typeable)

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Mul Expr Expr
          | Neg Expr
          deriving (Show, Eq, Data, Typeable)

constants :: Expr -> [Int]
constants x = nub [y | Val y <- universe x]
