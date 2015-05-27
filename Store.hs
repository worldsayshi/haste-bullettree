{-# LANGUAGE CPP, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Store (getTreeState,setTreeState) where
import EditorData


#ifndef __HASTE__
import Data.Acid
import Data.Acid.Remote

import Data.SafeCopy

import Data.Typeable

import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )

$(deriveSafeCopy 0 'base ''TreeFrame)

getTreeStateInt :: Query Tree Tree
getTreeStateInt = ask

setTreeStateInt :: Tree -> Update Tree ()
setTreeStateInt = put
  
$(makeAcidic ''TreeFrame ['getTreeStateInt, 'setTreeStateInt])


getTreeState = do
  acid <- openLocalState (Tree "root" []) -- exampleValue
  tree :: Tree <- query acid GetTreeStateInt
  closeAcidState acid
  return tree
  

setTreeState tree = do
  acid <- openLocalState exampleValue
  update acid (SetTreeStateInt tree)
  closeAcidState acid
  

#endif
#ifdef __HASTE__


e = error "Trying to access DB from client!"
setTreeState = e
getTreeState = e

#endif


getTreeState :: IO Tree
setTreeState :: Tree -> IO ()
