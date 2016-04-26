import Control.Monad

data Tree = EmptyTree | Node Int Tree Tree deriving (Show)
data Crumb = L Int Tree | R Int Tree deriving (Show)

type Crumbs = [Crumb]
type ZipperTree = (Tree, Crumbs)

goUp :: ZipperTree -> ZipperTree
goUp (l, L x r : cs) = (Node x l r, cs)
goUp (r, R x l : cs) = (Node x l r, cs)

goLeft :: ZipperTree -> ZipperTree
goLeft (Node x l r, cs) = (l, L x r : cs)

goRight :: ZipperTree -> ZipperTree
goRight (Node x l r, cs) = (r, R x l : cs)

getTree :: ZipperTree -> Tree
getTree (Node _ _ r, []) = r
getTree t = getTree . goUp $ t

singleton :: Int -> Tree
singleton x = Node x EmptyTree EmptyTree

insert :: ZipperTree -> Int -> ZipperTree
insert t@(Node x l r, cs) x' = 
  if x < x' then
    goRight (Node x l (Node x' r EmptyTree), cs)
  else
    goUp t `insert` x'

zipperRoot :: ZipperTree
zipperRoot = (singleton (-1), [])

printPrefix :: Tree -> IO ()
printPrefix EmptyTree = return ()
printPrefix (Node x l r) = do
  putStr $ show x
  putStr " "
  printPrefix l
  printPrefix r

solve :: [Int] -> IO ()
solve (n:xs) =
  unless (n == 0) $ do
    let descartesTree = getTree . foldl insert zipperRoot . take n $ xs
    printPrefix descartesTree
    putStrLn ""
    solve $ drop n xs

main :: IO ()
main = do
  input <- map read . words <$> getContents
  solve input
  return ()

