{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
import Haste.App
import Haste.Events
import Haste.DOM
import Haste.Perch
import Haste.LocalStorage
import qualified Haste.App.Concurrent as H
import qualified Control.Concurrent as C

import Lens.Family2

import Control.Monad (void,forM_)

import Prelude hiding (div)

--import GHC.Generics (Generic)

import EditorData



main = do
  runApp (mkConfig "localhost" 24602) $ do
    remoteTree <- liftServerIO $ C.newMVar (
      Tree "Root" [Tree "Subtree" []
                  ,Tree "another tree" [
                                       Tree "more subs" []]])
    
    trade <- remote $ \newTree -> do
      tree <- remoteTree
      liftIO $ do
        oldTree <- C.takeMVar tree
        C.putMVar tree newTree
        return oldTree
        
    getTree <- remote $ do
      tree <- remoteTree
      liftIO $ C.takeMVar tree

{-    changeTree <- remote $ \f -> do
      tree <- remoteTree
      liftIO $ do
        oldTree <- C.takeMVar tree
        let newTree = f oldTree
        C.putMVar tree newTree
        return oldTree
  -}
    runClient $ do

      currentTree <- onServer $ getTree :: Client Tree
      writeLog ("gotTree: "++show currentTree)

      H.fork $ let render tree ixs = do
                     div $ do
                       input
                         ! atr "type" "text"
                         ! atr "value" (tree ^. _text)
                         ! atr "data-index" (show ixs)
                       forM_ (zip [0..] (tree ^. _subtrees)) $ \(ix,subtree) -> do
                         render subtree (ixs++[ix])

                       addEvent this Change $ \_ -> do
                           writeLog "changed"
                       addEvent this KeyDown $ \keycode -> do
                         if keycode == 9
                              --- Need to use haste master to allow preventDefault!
                              -- Or maybe possible to just copy paste some of that code...
                           then do
                           liftIO $ preventDefault
                           writeLog "Tab!"
                           else writeLog $ "someOtherKey: " ++ show keycode
                       addEvent this KeyUp $ \keycode -> do
                         if keycode == 13
                           then writeLog "Enter!"
                           else writeLog $ "someOtherKey: " ++ show keycode
             in liftIO $ void $ withElem "root" $ build $ render currentTree []
