{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable, Rank2Types #-}
module EditorData where


import Data.Functor
import Control.Applicative

import Lens.Family2
import Haste.App
import GHC.Generics (Generic)

import Data.Data
import Data.Maybe
import Data.Typeable
import Data.Traversable (sequenceA)


data TreeFrame a = Tree {
  node :: a,
  subtrees :: [TreeFrame a]
  } deriving (Show,Eq,Read,Generic, Data, Typeable) --,Read,Generic)

_subtrees :: Lens' (TreeFrame a) [TreeFrame a]
_subtrees f (Tree txt subt) = (\subt' -> Tree txt subt') `fmap` (f subt)

-- _text' = exampleValue ^. _text
_subtrees' = exampleValue ^. _subtrees

instance Binary a => Binary (TreeFrame a)

-- Why is this not in the prelude??
(!!?) ::  [a] -> Int -> Maybe a
(!!?) l num
  | num >= length l || num < 0 = Nothing
  | otherwise = Just (l !! num)

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

_node :: Traversal' (TreeFrame a) a
_node f = (\(Tree nd sub) -> Tree <$> (f nd) <*> (pure sub))

_text :: Traversal' (Tree) String
_text = _node
_text' = exampleValue ^? _text

_elem :: Int -> Traversal' [a] a
_elem num f = changeAt num f -- undefined -- (\l -> )
_elem' = [1,2,3] ^? _elem 2

_last :: Traversal' [a] a
_last f l = _elem (length l - 1) f l


_init :: Traversal' [a] a
_init f l = sequenceA $ (fmap f (init l)) ++ (fmap pure ([last l]))

_textAt :: [Int] -> Traversal' (Tree) String
_textAt [] = _text
_textAt (num:nums) = _subtrees . (_elem num) . (_textAt nums)

_textAt' = exampleValue ^? _textAt [1,0]

_treeAt :: [Int] -> Traversal' (Tree) (Tree)
_treeAt [] f t = f t
_treeAt (num:nums) f t = (_subtrees . (_elem num) . (_treeAt nums)) f t

_treeAt' = exampleValue ^? _treeAt [1,0]


-- Ref to the recursive last node of the last child or itself if no children
_lastChild :: Traversal' (Tree) (Tree)
_lastChild f tree = if isJust (tree ^? _subtrees . _last)
                     then (_subtrees . _last . _lastChild) f tree
                     else (id) f tree

_lastChild' = exampleValue ^? _lastChild



-- | Modifications

type Tree = TreeFrame String







--instance Plated Tree where
--  plate f (Tree x xs) = Tree x <$> traverse f xs

-- | General purpose path traversal for Plated things
--path :: (Applicative f, Plated a) => [Int] -> LensLike' f a a
--path = foldr (\i l -> elementOf plate i . l) id

--text :: Lens' Tree String
--text f  (Tree txt subt) = (\txt' -> Tree txt' subt) <$> (f txt)




--makePrisms ''Tree
--makeLenses ''Tree

--_anelem :: forall (f :: * -> *) a. Applicative f =>  Int -> (a -> f a) -> [a] -> f [a]

--_2text :: Traversal' Tree String
--_2text num f = (\(Tree txt sub) -> Tree <$> (pure txt) <*> (sub))
--_2text' =  exampleValue ^? _2text 1 -- Just "c"



--texts tree = [text | Tree text _ <- universe tree]
--texts' = texts exampleValue

--appendTo tree txt = transform (\(Tree txt' sub) -> Tree (txt' ++ txt) sub ) tree 
--appendTo' = appendTo exampleValue "Foo" 

--_texts :: Traversal' Tree String
--_texts f tree = transform (fooo f) tree -- undefined --transform f tree


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
