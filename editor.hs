{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
import Haste.App
import Haste.Events
import Haste.DOM
import Haste.Perch
import Haste.LocalStorage
import qualified Haste.App.Concurrent as H
import qualified Control.Concurrent as C

import Control.Monad (void)

import Prelude hiding (div)

--import GHC.Generics (Generic)

import EditorData



main = do
  runApp (mkConfig "localhost" 24602) $ do
    remoteTree <- liftServerIO $ C.newMVar (Tree "Home" [])
    
    trade <- remote $ \newTree -> do
      tree <- remoteTree
      liftIO $ do
        oldTree <- C.takeMVar tree
        C.putMVar tree newTree
        return oldTree
        
    getTree <- remote $ do
      tree <- remoteTree
      liftIO $ C.takeMVar tree
    
    runClient $ do

      tree <- onServer $ getTree :: Client Tree
      writeLog ("gotTree: "++show tree)

      liftIO $ void $ withElem "root" $ build $ do
        div $ do
          input ! atr "type" "text"
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
