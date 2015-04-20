{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
import Haste.App
import Haste.Events
import Haste.DOM
import GHC.Generics (Generic)
import qualified Haste.App.Concurrent as H
import qualified Control.Concurrent as C
import Haste.LocalStorage
import Control.Monad (void)

--import Lens.Family2
--import Lens.Family2.Unchecked

import Haste.Perch
import Prelude hiding (div)

import Lens.Family2
import Lens.Family2.Unchecked



data Tree = Tree {
  text :: String,
  subtrees :: [Tree]
  } deriving (Show,Eq,Read,Generic)


{-
text :: Lens' Tree String
text f (Tree text' subtrees') =
  (\text'' -> Tree text'' subtrees') `fmap` (f text')

subtrees :: Lens' Tree [Tree]
subtrees f (Tree text' subtrees') =
  (\subtrees'' -> Tree text' subtrees'') `fmap` (f subtrees')
-}

getTextAt :: [Int] -> Tree -> String
setTreeAt ::[Int] -> Tree -> Tree
modifyTreeAt :: [Int] -> (Tree -> Tree) -> Tree

getTextAt [] tree = text tree
getTextAt (ix:ixs) tree = getTextAt ixs ((subtrees tree) !! ix)
setTreeAt = undefined
modifyTreeAt = undefined

ixs :: Int -> [a] -> Maybe a
ixs i l = if length l < i
          then Nothing
          else Just (l !! i)


--anum :: Int
--anum = [0,1]^.at 1
{-
getTextAt [] tree = text tree
getTextAt (ix:ixs) tree = getTextAt ixs ((subtrees tree) !! ix)

setTreeAt [] tree = tree
setTreeAt (ix:ixs) tree =
  where replaceNth e n l = x ++ e : xs
        where (x,_:xs) = slitAt n l
  setTreeAt ixs ((subtrees tree) !! ix)
  

modifyTreeAt [] f = 
-}
instance Binary Tree

--loadFromStorage :: CIO Tree
{-loadFromStorage = do
  eTree <- liftIO $ getItem "tree"
  return $ case eTree of
    Right todos  -> todos
    Left _       -> []
-}

-- build'= flip build


main2 :: IO ()
main2 = do
  runApp (mkConfig "ws://localhost:24602" 24602) $ do
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
--            p "hello"
--            p ! atr "style" "color:red" $ "world"
      return ()



main :: IO ()
main = do
  runApp (mkConfig "ws://localhost:24602" 24602) $ do
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
      --tree <- onServer $ getTree :: Client Tree
      liftIO $ withElem "root" $ build $ do
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
      return ()
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
