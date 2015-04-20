{-# LANGUAGE TemplateHaskell #-}
module EditorData where

import Data.Functor
import Control.Applicative

import Control.Lens
import Control.Lens.At
import Data.List.Lens

data Tree = Tree {_text :: String,_subtrees :: [Tree]} deriving (Show,Eq) --,Read,Generic)

makePrisms ''Tree
makeLenses ''Tree


instance Plated Tree where
  plate f (Tree x xs) = Tree x <$> traverse f xs

-- | General purpose path traversal for Plated things
path :: (Applicative f, Plated a) => [Int] -> LensLike' f a a
path = foldr (\i l -> elementOf plate i . l) id


exampleValue =
  Tree "a"
    [ Tree "b" []
    , Tree "c"
       [Tree "d"
         []
       ]
    ]

demo = demo1 == Just (Tree "d" [])

demo1 = preview (path [1,0]) exampleValue
demo2 = exampleValue ^? path [1,0] . text

{-
example = Tree "foo" [Tree "bar" []]

--aix = ix 0

achild [] = example ^? text
achild n:ns = 


child = (example ^? subtrees . ix 1 . text) ^. _Just

wot = (Nothing :: Maybe ()) ^. _Just



-}
