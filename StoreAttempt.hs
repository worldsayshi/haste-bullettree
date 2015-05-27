{-# LANGUAGE CPP, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module StoreAttempt where

import Data.Acid
import Data.Acid.Remote
import Data.SafeCopy
import Data.Typeable
import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )

data TreeGraph node = TreeGraph node [TreeGraph node] deriving (Typeable,Show)

data Node = Node {
  id :: Int,
  text :: String
  } deriving (Typeable,Show)

data Tree = Tree (TreeGraph Node) deriving (Typeable,Show)

$(deriveSafeCopy 0 'base ''Node)
$(deriveSafeCopy 0 'base ''TreeGraph)
$(deriveSafeCopy 0 'base ''Tree)

getStateInt :: Query Tree Tree
getStateInt = ask

setStateInt :: Tree -> Update Tree ()
setStateInt = put
  
$(makeAcidic ''Tree ['getStateInt, 'setStateInt])

nullTree = Tree $ TreeGraph (Node 0 "root") []

getState = do
  acid <- openLocalState nullTree
  tree <- query acid GetStateInt
  closeAcidState acid
  return tree
  

setState tree = do
  acid <- openLocalState nullTree
  update acid (SetStateInt tree)
  closeAcidState acid
  
