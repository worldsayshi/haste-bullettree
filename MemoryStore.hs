{-# LANGUAGE  CPP #-}
module MemoryStore where

import qualified Control.Concurrent as C
import Haste.App hiding (set)
import EditorData

import Store

#ifndef __HASTE__
import Control.Concurrent.Event
#endif
#ifdef __HASTE__
set = undefined
new = undefined
wait = undefined
data Event = Event
#endif

data State = State {
  trigger :: Event,
  stateMVar :: C.MVar Tree
  }


initState = liftServerIO $ do
  persistedTree <- getTreeState
  changeTrigger <- new
  stateMVar <- C.newMVar persistedTree
  return $ State changeTrigger stateMVar


data Store = Store {
  setTree :: Remote (Tree -> Server ()),
  getNextTree :: Remote (Server Tree),
  getCurrentTree :: Remote (Server Tree)
  }

openStore :: App Store
openStore = do
  retrieveState <- initState

  setTree' <- remote $ \newTree -> do
    state <- retrieveState
    liftIO $ do
--        oldTree <- C.takeMVar $ stateMVar state
        C.modifyMVar_ (stateMVar state) (\_->return newTree)
        --C.swapMVar (stateMVar state) newTree
        set $ trigger state
        setTreeState newTree

  getCurrentTree' <- remote $ do
    state <- retrieveState
    liftIO $ C.readMVar (stateMVar state)
    
  getNextTree' <- remote $ do
    state <- retrieveState
    liftIO $ do
      wait $ trigger state
    liftIO $ C.readMVar (stateMVar state)

  return $ Store setTree' getNextTree' getCurrentTree'
