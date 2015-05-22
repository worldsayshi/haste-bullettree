{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, CPP, RankNTypes #-}
module Main (main) where
import Haste.App
import Haste.Events
import Haste.DOM
import Haste.App.Perch hiding (id)
import Haste.LocalStorage
import qualified Haste.App.Concurrent as H

import Lens.Family2
import Lens.Family2.Stock

import Control.Monad (void,forM_)
import Control.Applicative
import Data.Maybe (fromJust)


import Prelude hiding (div,span)

--import GHC.Generics (Generic)

import EditorData
import LocalStore
import qualified MemoryStore as Mem


main :: IO ()

main = do
  runApp (mkConfig "localhost" 24602) $ do

    store <- Mem.openStore

    runClient $ do
      cache <- localCache store
      writeLog "fetching tree"
      currentTree <- getCurrentTree cache -- onServer $ getCurrentTree store :: Client Tree
      writeLog ("gotTree: "++show currentTree)

      H.fork $ let
        awaitLoop currentTree = do
          void $ withElem "editorRoot" $ \elem -> do
            clearChildren elem
            build (render currentTree currentTree []) elem
          currentTree' <- getNextTree cache -- onServer $ getNextTree store :: Client Tree
          awaitLoop currentTree'
        render = renderTree cache
        in awaitLoop currentTree

renderTree cache allTree tree ixs = do
  div ! atr "class" "tree-container" $ do
    span ! atr "class" "bullet" $ "›"
    span ! atr "class" "input-wrapper" $ do
      (((input
        ! atr "type" "text"
        ! atr "value" (tree ^. _text)
        ! atr "data-index" (show ixs)
        `addEvent`
          Change $ \ev -> updateWithInputValue allTree ixs cache)
        `addEvent` KeyDown $ \keydata -> do
           writeLog $ "KeyDown: " ++ show keydata
           case keyCode keydata of
             9 -> (liftIO preventDefault)
                  >> if keyShift keydata
                     then deindentNode allTree ixs cache
                     else indentNode allTree ixs cache
             38 -> (liftIO preventDefault) >> moveCursorUp allTree ixs -- writeLog "Up arrow!"
             40 -> (liftIO preventDefault) >> moveCursorDown allTree ixs
             -- Todo check also if it has child trees. Then abort.
             8 -> if allOf (_textAt ixs) (=="") allTree
                  then (liftIO preventDefault) >> removeNodeAt allTree ixs cache
                  else return ()
             _ -> return ())
            
        `addEvent` KeyUp $ \keydata -> do
          writeLog $ "KeyUp: " ++ show keydata
          case keyCode keydata of
            9 -> (liftIO preventDefault)
            13 -> (liftIO preventDefault) >> insertNodeAfter allTree ixs cache
            38 -> (liftIO preventDefault) >> writeLog "Up arrow!"
            40 -> (liftIO preventDefault) >> writeLog "Down arrow!"
            _  -> return ())
    forM_ (zip [0..] (tree ^. _subtrees)) $ \(ix,subtree) -> do
      renderTree cache allTree subtree (ixs++[ix])


{-- ACTIONS --}

--navTo :: (MonadIO m, Show a, Functor m) => [a] -> m ()
navTo ixs = do
  -- TODO sometimes getting pattern match error here, can't reproduce...
  elem:[] <- elemsByQS documentBody $ "input[data-index=\""++show ixs++"\"]"
  focus elem

updateWithInputValue tree ixs cache = do
  els <- elemsByQS document ("input[data-index=\""++show ixs++"\"]")
  forM_ els (
    \el -> do
      val <- getProp el "value"
      let newTree = tree & _textAt ixs .~ val
      writeLog ("result: "++val)
      setTree cache newTree)

      -- onServer $ (setTree store) <.> (newTree))

--moveCursorUp :: (MonadIO m, Functor m) => [Int] -> m ()
      
moveCursorUp tree ixs =
  let findIxsNodeAbove tree ixs =
        let subtrees = tree ^? _treeAt ixs . _subtrees
        in case (subtrees ^? _Just . _last) of
          (Just _) -> findIxsNodeAbove tree $ ixs ++ [(length $ fromJust $ subtrees) - 1]
          Nothing -> ixs
      parentIxs = init ixs
  in case ixs ^? _last of
    (Just 0) -> navTo $ init ixs
    (Just ix) -> navTo $ findIxsNodeAbove tree (parentIxs++[ix-1])
    _ -> return ()


moveCursorDown tree ixs = do
  let findIxsNodeBelow tree [] = Nothing
      findIxsNodeBelow tree ixs =
        let parentIxs = init ixs
            fringeIx = last ixs
            thisIxs = (parentIxs ++ [fringeIx+1])
            maybeNodeBelow  = tree ^? _treeAt thisIxs
          in case maybeNodeBelow of
          (Just node) -> Just thisIxs
          Nothing -> findIxsNodeBelow tree parentIxs
      childIxs = ixs++[0]
      maybeChildNode = tree ^? _treeAt childIxs
   in case maybeChildNode of
    Just childNode -> navTo childIxs
    _ -> case findIxsNodeBelow tree ixs of
      Just ixs' -> navTo ixs'
      Nothing -> return ()
        
insertNodeAfter tree ixs cache = do
  let parentIxs = init ixs
      fringeIx = last ixs
      newTree = insert tree ixs (Tree "" [])
      newIxs = parentIxs ++ [fringeIx + 1]
  setTree cache tree -- onServer $ (setTree store) <.> (newTree)
  -- This navTo should be scheduled to react to a rerended event?
  -- Timing problems can occur
  navTo newIxs

insert tree ixs node =
  let parentIxs = init ixs
      fringeIx = last ixs
  in tree & _treeAt parentIxs %~
     (\parentTree ->
       parentTree & _subtrees %~
       (\trees ->
         let nextIx = fringeIx + 1
             -- Can just as well allow inserting a list of nodes
         in take nextIx trees ++ [node] ++ drop nextIx trees))

insertLast tree ixs node =
  tree & _treeAt ixs %~
  (\parentTree -> parentTree & _subtrees %~
                  (\trees -> trees++[node]))

pluck tree ixs =
  let parentIxs = init ixs
      fringeIx = last ixs
  in
   (tree & _treeAt parentIxs %~
    (\parentTree ->
      parentTree & _subtrees %~
      (\trees ->
        take fringeIx trees ++ drop (fringeIx+1) trees)),
    tree ^? _treeAt ixs)

removeNodeAt tree ixs cache = do
  let parentIxs = init ixs
      fringeIx = last ixs
      (newTree,_) = pluck tree ixs
                    {-tree & _treeAt parentIxs %~
                (\parentTree ->
                  parentTree & _subtrees %~
                  (\trees ->
                    take fringeIx trees ++ drop (fringeIx+1) trees))-}
  moveCursorUp tree ixs
  setTree cache newTree
--  onServer $ (setTree store) <.> (newTree)

deindentNode tree ixs cache = do
  undefined

indentNode tree ixs cache = do
  let parentIxs = init ixs
      fringeIx = last ixs
      (newIxs, newTree) =
        if fringeIx == 0
        then (ixs, tree)
        else (parentIxs++[fringeIx-1],
              fromJust $ do
                let (tree', mnode) = pluck tree ixs
                node <- mnode
                return $ insertLast tree' (parentIxs++[fringeIx-1]) node)
  setTree cache newTree
    -- onServer $ (setTree store) <.> (newTree)
  navTo newIxs
      
