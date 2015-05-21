{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, CPP, RankNTypes #-}
module Main (main) where
import Haste.App
import Haste.Events
import Haste.DOM
import Haste.App.Perch
import Haste.LocalStorage
import qualified Haste.App.Concurrent as H

import Lens.Family2

import Control.Monad (void,forM_)
import Control.Applicative
import Data.Maybe (fromJust)


import Prelude hiding (div,span)

--import GHC.Generics (Generic)

import EditorData
import MemoryStore


main :: IO ()

main = do
  runApp (mkConfig "localhost" 24602) $ do

    store <- openStore

    runClient $ do
      writeLog "fetching tree"
      currentTree <- onServer $ getCurrentTree store :: Client Tree
      writeLog ("gotTree: "++show currentTree)

      H.fork $ let
        awaitLoop currentTree = do
          void $ withElem "editorRoot" $ \elem -> do
            clearChildren elem
            build (render currentTree currentTree []) elem
          currentTree' <- onServer $ getNextTree store :: Client Tree
          awaitLoop currentTree'
        render = renderTree store
        in awaitLoop currentTree

renderTree store allTree tree ixs = do
  div ! atr "class" "tree-container" $ do
    span ! atr "class" "bullet" $ "›"
    span ! atr "class" "input-wrapper" $ do
      (((input
        ! atr "type" "text"
        ! atr "value" (tree ^. _text)
        ! atr "data-index" (show ixs)
        `addEvent`
          Change $ \ev -> updateWithInputValue allTree ixs store)
        `addEvent` KeyDown $ \keycode -> do
           case keycode of
             9 -> (liftIO preventDefault) >> indentNode allTree ixs store
             38 -> (liftIO preventDefault) >> moveCursorUp allTree ixs -- writeLog "Up arrow!"
             40 -> (liftIO preventDefault) >> moveCursorDown allTree ixs
             _ -> writeLog $ "KeyDown: " ++ show keycode) 
            
        `addEvent` KeyUp $ \keycode -> do
          case keycode of
            13 -> (liftIO preventDefault) >> insertNodeAfter allTree ixs store
            38 -> (liftIO preventDefault) >> writeLog "Up arrow!"
            40 -> (liftIO preventDefault) >> writeLog "Down arrow!"
            -- Todo check also if it has child trees. Then abort.
            8 -> if allOf (_textAt ixs) (=="") allTree
                 then (liftIO preventDefault) >> removeNodeAt allTree ixs store
                 else return ()
            _  -> writeLog $ "KeyUp: " ++ show keycode)
    forM_ (zip [0..] (tree ^. _subtrees)) $ \(ix,subtree) -> do
      renderTree store allTree subtree (ixs++[ix])


{-- ACTIONS --}

--navTo :: (MonadIO m, Show a, Functor m) => [a] -> m ()
navTo ixs = do
  -- TODO sometimes getting pattern match error here, can't reproduce...
  elem:[] <- elemsByQS documentBody $ "input[data-index=\""++show ixs++"\"]"
  focus elem

updateWithInputValue tree ixs store = do
  els <- elemsByQS document ("input[data-index=\""++show ixs++"\"]")
  forM_ els (
    \el -> do
      val <- getProp el "value"
      let newTree = tree & _textAt ixs .~ val
      writeLog ("result: "++val)
      onServer $ (setTree store) <.> (newTree))

--moveCursorUp :: (MonadIO m, Functor m) => [Int] -> m ()
moveCursorUp tree ixs =
  case ixs ^? _last of
    (Just 0) -> navTo $ init ixs
    (Just ix) -> navTo $ (init ixs) ++ [ix - 1]
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
        
insertNodeAfter tree ixs store = do
  let parentIxs = init ixs
      fringeIx = last ixs
      newTree = insert tree ixs (Tree "" [])
      newIxs = parentIxs ++ [fringeIx + 1]
  onServer $ (setTree store) <.> (newTree)
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

removeNodeAt tree ixs store = do
  let parentIxs = init ixs
      fringeIx = last ixs
      (newTree,_) = pluck tree ixs
                    {-tree & _treeAt parentIxs %~
                (\parentTree ->
                  parentTree & _subtrees %~
                  (\trees ->
                    take fringeIx trees ++ drop (fringeIx+1) trees))-}
  moveCursorUp tree ixs
  onServer $ (setTree store) <.> (newTree)

deindentNode tree ixs store = do
  undefined

indentNode tree ixs store = do
  let parentIxs = init ixs
      fringeIx = last ixs
      newTree = fromJust $ case fringeIx of
        0 -> return tree
        n -> do
          let (tree', mnode) = pluck tree ixs
          node <- mnode
          return $ insertLast tree' (parentIxs++[n-1]) node
  onServer $ (setTree store) <.> (newTree)

      
