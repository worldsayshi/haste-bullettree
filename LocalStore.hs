module LocalStore where

import EditorData
import Haste.App
import MemoryStore as Mem

data Cache = Cache {
  setTree :: Tree -> Client (),
  getNextTree :: Client Tree,
  getCurrentTree :: Client Tree
  }

localCache :: Store -> Client Cache
localCache store = do
  let setTree' tree = onServer $ (Mem.setTree store) <.> (tree)
      getCurrentTree' = onServer $ Mem.getCurrentTree store
      getNextTree' = onServer $ Mem.getNextTree store
  
  return $ Cache setTree' getNextTree' getCurrentTree'
