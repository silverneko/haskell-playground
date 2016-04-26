import Control.Monad
import Text.Printf
import System.Random (randomIO)
import Control.Applicative
import qualified Data.Tree as Tree
-- you may need to do `cabal install pretty-tree`
import Data.Tree.Pretty (drawVerticalTree)

randIntIO :: IO Int
randIntIO = randomIO :: IO Int

data Treap a = EmptyTreap | Treap {pri :: Int, key :: a, kSum :: a, size :: Int, lchild :: Treap a, rchild :: Treap a} deriving (Show)

leaf :: Treap a
leaf = EmptyTreap

treap :: (Num a) => a -> IO (Treap a)
treap e = do
  p <- randIntIO
  return $ Treap p e e 1 leaf leaf

pull :: (Num a) => Treap a -> Treap a
pull EmptyTreap = EmptyTreap
pull a@(Treap {key = k, lchild = lt, rchild = rt}) =
  a {kSum = k + kSum' lt + kSum' rt, size = 1 + size' lt + size' rt}
  where kSum' EmptyTreap = 0
        kSum' a = kSum a
        size' EmptyTreap = 0
        size' a = size a

heuristicMerge :: (Num a, Ord a) => Treap a -> Treap a -> IO (Treap a)
heuristicMerge EmptyTreap b = return b
heuristicMerge a EmptyTreap = return a
heuristicMerge a b = if size a < size b then heuristicMerge b a else do
  t1 <- insert a $ key b
  t2 <- heuristicMerge t1 $ lchild b
  heuristicMerge t2 $ rchild b

insert :: (Num a, Ord a) => Treap a -> a -> IO (Treap a)
insert EmptyTreap e = treap e
insert a e = do
  mt <- treap e
  let (lt, rt) = split a e
  return $ merge lt $ merge mt rt

merge :: (Num a) => Treap a -> Treap a -> Treap a
merge a EmptyTreap = a
merge EmptyTreap a = a
merge lt rt = if pri lt < pri rt
  then pull $ lt {rchild = merge (rchild lt) rt}
  else pull $ rt {lchild = merge lt (lchild rt)}

split :: (Num a, Ord a) => Treap a -> a -> (Treap a, Treap a)
split EmptyTreap _ = (leaf, leaf)
split a k = if key a < k
  then let (lt, rt) = split (rchild a) k
       in (pull $ a {rchild = lt}, rt)
  else let (lt, rt) = split (lchild a) k
       in (lt, pull $ a {lchild = rt})

toTree :: (Show a) => Treap a -> Tree.Tree String
toTree EmptyTreap = Tree.Node "nil" []
toTree a = Tree.Node (show $ (key a, kSum a)) $ toTree <$> [lchild a, rchild a]

drawTreap :: (Show a) => Treap a -> String
drawTreap = drawVerticalTree . toTree
--drawTreap = Tree.drawTree . toTree

main = do
  numbers <- map read . words <$> getLine :: IO [Int]
  t1 <- foldM insert leaf numbers
  putStrLn $ drawTreap t1
  numbers <- map read . words <$> getLine :: IO [Int]
  t2 <- foldM insert leaf numbers
  putStrLn $ drawTreap t2
  t3 <- heuristicMerge t1 t2
  putStrLn $ drawTreap t3
  return ()

