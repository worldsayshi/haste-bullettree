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
          Change $ \ev -> do
          els <- elemsByQS document ("input[data-index=\""++show ixs++"\"]")
          forM_ els (
            \el -> do
              val <- getProp el "value"
              let newTree = allTree & _textAt ixs .~ val
              writeLog ("result: "++val)
              onServer $ (setTree store) <.> (newTree)))
        `addEvent` KeyDown $ \keycode -> do
           case keycode of
             9 -> (liftIO preventDefault) >> writeLog "Tab!"
             38 -> (liftIO preventDefault) >> moveCursorUp allTree ixs -- writeLog "Up arrow!"
             40 -> (liftIO preventDefault) >> moveCursorDown allTree ixs
             _ -> writeLog $ "KeyDown: " ++ show keycode) 
            
        `addEvent` KeyUp $ \keycode -> do
          case keycode of
            13 -> writeLog "Enter!"
            38 -> (liftIO preventDefault) >> writeLog "Up arrow!"
            40 -> (liftIO preventDefault) >> writeLog "Down arrow!"
            _  -> writeLog $ "KeyUp: " ++ show keycode)
    forM_ (zip [0..] (tree ^. _subtrees)) $ \(ix,subtree) -> do
      renderTree store allTree subtree (ixs++[ix])

--navTo :: (MonadIO m, Show a, Functor m) => [a] -> m ()
navTo ixs = do
  -- TODO sometimes getting pattern match error here, can't reproduce...
  elem:[] <- elemsByQS documentBody $ "input[data-index=\""++show ixs++"\"]"
  focus elem

--moveCursorUp :: (MonadIO m, Functor m) => [Int] -> m ()
moveCursorUp tree ixs =
  case ixs ^? _last of
    (Just 0) -> navTo $ init ixs
    (Just ix) -> navTo $ (init ixs) ++ [ix - 1]
    _ -> return ()

moveCursorDown tree ixs = do
  let parentIxs = init ixs
      parentNode = fromJust $ tree ^? _treeAt parentIxs
      childIxs = ixs++[0]
      maybeChildNode = tree ^? _treeAt childIxs
  -- check case where itself has children! Then navigate to the first?
  case maybeChildNode of
    Just childNode -> navTo childIxs
    _ -> do
      let n = last ixs :: Int
      if n >= length (parentNode ^. _subtrees)
        then navTo ((init parentIxs) ++ [(last parentIxs) + 1])
        else navTo (init ixs ++ [n+1])
        
  
