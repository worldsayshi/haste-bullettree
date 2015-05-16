{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, CPP #-}
import Haste.App
import Haste.Events
import Haste.DOM
import Haste.App.Perch
import Haste.LocalStorage
import qualified Haste.App.Concurrent as H
import qualified Control.Concurrent as C

import Lens.Family2

import Control.Monad (void,forM_)
import Control.Applicative



import Prelude hiding (div)

--import GHC.Generics (Generic)

import EditorData


import Store



main :: IO ()
main2 = do
  runApp (mkConfig "localhost" 24602) $ do
    runClient $ do
      let
        render :: [Int] -> Elem -> Client ()
        render [] elem = return ()
        render (x:xs) elem = do
          el <- newElem "input"
          onEvent el Change (\_ -> do
                                 writeLog (show x)
                             )
          addChild el elem
          render xs elem
        in withElem "root" (render [1,2,3])

main3 = do
  runApp (mkConfig "localhost" 24602) $ do
    runClient $ do
      let
        render :: [Int] -> Perch
        render [] = return ()
        render (x:xs) = do
          input `addEvent` Change $ (\_ -> do
                                 writeLog (show x)
                             )
          render xs
        in void $ withElem "root" $ build (render [1,2,3])

main4 = do
  runApp (mkConfig "localhost" 24602) $ do
    runClient $ do
      let
        render :: [Int] -> Perch
        render [] = return ()
        render (x:xs) = do
          div $ do
            input
            addEvent this Change $ (\_ -> do
                                 writeLog (show x)
                             )

          render xs
        in void $ withElem "root" $ build (render [1,2,3])


main = do
  runApp (mkConfig "localhost" 24602) $ do
    remoteTree <- liftServerIO $ do
      tree <- getTreeState
      changedTrigger <- C.newMVar True
      mtree <- C.newMVar tree
      return (changedTrigger, mtree)

    trade <- remote $ \newTree -> do

      (changedTrigger,tree) <- remoteTree
      liftIO $ do
        print "putting"
--        oldTree <- C.takeMVar tree
        C.swapMVar tree newTree
        print "put1"
        C.swapMVar changedTrigger True
        print "put2"
        setTreeState newTree
        
--        return oldTree

    readTree <- remote $ do
      (_,mtree) <- remoteTree
      liftIO $ print "read"
      res <- liftIO $ C.readMVar mtree
      liftIO $ print "read2"
      return res
      

    getTree <- remote $ do
      liftIO $ print "1"
      (changedTrigger,mtree) <- remoteTree
      liftIO $ print "2"
      liftIO $ C.takeMVar changedTrigger
      res <- liftIO $ C.readMVar mtree
      liftIO $ print "3"
      return res
      
{-    changeTree <- remote $ \f -> do
      tree <- remoteTree
      liftIO $ do
        oldTree <- C.takeMVar tree
        let newTree = f oldTree
        C.putMVar tree newTree
        return oldTree
  -}
    runClient $ do
      writeLog "fetching tree"
      currentTree <- onServer $ readTree :: Client Tree
      writeLog ("gotTree: "++show currentTree)

      H.fork $ let
        awaitLoop currentTree = do
          void $ withElem "root" $ \elem -> do
            clearChildren elem
            build (render currentTree currentTree []) elem
          currentTree' <- onServer $ getTree :: Client Tree
          awaitLoop currentTree'
        render allTree tree ixs = do
          div $ do
            input
              ! atr "type" "text"
              ! atr "value" (tree ^. _text)
              ! atr "data-index" (show ixs) `addEvent` Change $ \ev -> do
                
              

              els <- elemsByQS document ("input[data-index=\""++show ixs++"\"]")
              forM_ els ( \el -> do
                        val <- getProp el "value"
                        let newTree = allTree & _textAt ixs .~ val
                        writeLog ("result: "++val)
                        onServer $ trade <.> (newTree))
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
            forM_ (zip [0..] (tree ^. _subtrees)) $ \(ix,subtree) -> do
              render allTree subtree (ixs++[ix])
        in awaitLoop currentTree

