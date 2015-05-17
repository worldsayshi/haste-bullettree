{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, CPP, RankNTypes #-}
import Haste.App
import Haste.Events
import Haste.DOM
import Haste.App.Perch
import Haste.LocalStorage
import qualified Haste.App.Concurrent as H

import Lens.Family2

import Control.Monad (void,forM_)
import Control.Applicative



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
          if keycode == 9
             --- Need to use haste master to allow preventDefault!
             -- Or maybe possible to just copy paste some of that code...
            then do
            liftIO $ preventDefault
            writeLog "Tab!"
            else writeLog $ "someOtherKey: " ++ show keycode)
        `addEvent` KeyUp $ \keycode -> do
          if keycode == 13
            then writeLog "Enter!"
            else writeLog $ "someOtherKey: " ++ show keycode)
    forM_ (zip [0..] (tree ^. _subtrees)) $ \(ix,subtree) -> do
      renderTree store allTree subtree (ixs++[ix])

