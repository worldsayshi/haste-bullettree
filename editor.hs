{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
import Haste.App
import Haste.Events
import Haste.DOM
import Haste.Perch
import Haste.LocalStorage
import qualified Haste.App.Concurrent as H
import qualified Control.Concurrent as C

import Control.Monad (void)

-- TODO: For some reason hastec is using an older version of base. This might cause trouble.
-- I can't install lens with haste-inst. Not entirely sure is related
-- IF I can't get lens to work with haste, try with lens-family again and use inspiration from lens!


import Prelude hiding (div)

--import GHC.Generics (Generic)

import EditorData

--loadFromStorage :: CIO Tree
{-loadFromStorage = do
  eTree <- liftIO $ getItem "tree"
  return $ case eTree of
    Right todos  -> todos
    Left _       -> []
-}

-- build'= flip build


main :: IO ()
main2 = do
  runApp (mkConfig "localhost" 24602) $ do
    remoteTree <- liftServerIO $ C.newMVar (Tree "Home" [])
    
    trade <- remote $ \newTree -> do
      tree <- remoteTree
      liftIO $ do
        oldTree <- C.takeMVar tree
        C.putMVar tree newTree
        return oldTree
    
    runClient $ do
      liftIO $ void $ withElem "root" $ build $ do
        div $ do
          addEvent this Click $ \_ -> alert "hello, world!"
          div $ do
            addEvent this Click $ \_ -> alert "hello, world!"
            p "hello"
            p ! atr "style" "color:red" $ "world"
      return ()




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
      
      -- Maybe use https://github.com/agocorona/haste-perch for virtual dom like things
      -- Nope, doesn't support haste 0.5 just yet. Maybe patch it? 6 hours ago (now) an issue on new event system was reported PATCHED!
      writeLog "start"
      tree <- onServer $ getTree :: Client Tree
      writeLog ("gotTree: "++show tree)
      liftIO $ void $ withElem "root" $ build $ do
        writeLog "build"
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


          -- $ do -- ! (atr "type" "text")
          {-addEvent this KeyDown $ \keycode -> do
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
              else writeLog $ "someOtherKey: " ++ show keycode-}
      
          {-
          elem <- newElem "input"
          setAttr elem "type" "text"
          elem `onEvent` Change $ \_ -> do
            writeLog "changed"
          elem `onEvent` KeyDown $ \keycode -> do
            if keycode == 9
               --- Need to use haste master to allow preventDefault!
               -- Or maybe possible to just copy paste some of that code...
              then do
              liftIO $ preventDefault
              writeLog "Tab!"
              else writeLog $ "someOtherKey: " ++ show keycode
          elem `onEvent` KeyUp $ \keycode -> do
            if keycode == 13
              then writeLog "Enter!"
              else writeLog $ "someOtherKey: " ++ show keycode
          addChild elem root
-}
