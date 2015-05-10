{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable, Rank2Types #-}
module EditorData where


import Data.Functor
import Control.Applicative

import Lens.Family2
--import Lens.Family2.Stock
--import Control.Lens
--import Control.Lens.At
--import Data.List.Lens

import Haste.App
import GHC.Generics (Generic)

import Data.Data
import Data.Typeable
import Data.Traversable (sequenceA)
--import Data.Generics.Uniplate.Data
--import Data.Generics.Uniplate

data Tree = Tree {
  _text :: String,
  _subtrees :: [Tree]
  } deriving (Show,Eq,Read,Generic, Data, Typeable) --,Read,Generic)

text :: Lens' Tree String
text f  (Tree txt subt) = (\txt' -> Tree txt' subt) <$> (f txt)
subtrees :: Lens' Tree [Tree]
subtrees f (Tree txt subt) = (\subt' -> Tree txt subt') `fmap` (f subt)

text' = exampleValue ^. text 
subtrees' = exampleValue ^. subtrees

--makePrisms ''Tree
--makeLenses ''Tree

instance Binary Tree

-- Why is this not in the prelude??
(!!?) ::  [a] -> Int -> Maybe a
(!!?) l num
  | num >= length l || num < 0 = Nothing
  | otherwise = Just (l !! num)

--instance Plated Tree where
--  plate f (Tree x xs) = Tree x <$> traverse f xs

-- | General purpose path traversal for Plated things
--path :: (Applicative f, Plated a) => [Int] -> LensLike' f a a
--path = foldr (\i l -> elementOf plate i . l) id

foldrWithId f = foldr f id 

exampleValue =
  Tree "a"
    [ Tree "b" []
    , Tree "c"
       [Tree "d"
         []
       ]
    ]


changeAt num f l =
  sequenceA
  $ map snd
  $ map (\(num', elem) ->
        if num == num'
        then (num',f elem)
        else (num',pure elem)) $ zip [0..] l


changeAt' = changeAt 2 (\i -> Just (-i)) [1,2,3]

--texts tree = [text | Tree text _ <- universe tree]
--texts' = texts exampleValue

--appendTo tree txt = transform (\(Tree txt' sub) -> Tree (txt' ++ txt) sub ) tree 
--appendTo' = appendTo exampleValue "Foo" 

--_texts :: Traversal' Tree String
--_texts f tree = transform (fooo f) tree -- undefined --transform f tree
_atext :: Traversal' Tree String
_atext f = (\(Tree txt sub) -> Tree <$> (f txt) <*> (pure sub))
_atext' = exampleValue ^? _atext
_anelem :: Int -> Traversal' [a] a
--_anelem :: forall (f :: * -> *) a. Applicative f =>  Int -> (a -> f a) -> [a] -> f [a]
_anelem num f = changeAt num f -- undefined -- (\l -> )
_anelem' = [1,2,3] ^? _anelem 2

_textAt :: [Int] -> Traversal' Tree String
_textAt [] = _atext
_textAt (num:nums) = subtrees . (_anelem num) . (_textAt nums)

_textAt' = exampleValue ^? _textAt [1,0]
--_2text :: Traversal' Tree String
--_2text num f = (\(Tree txt sub) -> Tree <$> (pure txt) <*> (sub))
--_2text' =  exampleValue ^? _2text 1 -- Just "c"



--_texts' f tree = transform f tree

--demo = demo1 == Just (Tree "d" [])

--demo1 = preview (path [1,0]) exampleValue
--demo2 = exampleValue ^? path [1,0] . text

{-
example = Tree "foo" [Tree "bar" []]

--aix = ix 0

achild [] = example ^? text
achild n:ns = 


child = (example ^? subtrees . ix 1 . text) ^. _Just

wot = (Nothing :: Maybe ()) ^. _Just



-}
