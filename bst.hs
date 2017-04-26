{- BINARY SEARCH TREE IMPLEMENTATION -}

module Tree 
  ( Tree(..)
  , pretty
  , size
  , depth
  , empty
  , contains
  , insert 
  , delete
  , fromList
  , toList
  , balance
  ) where


data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Read,Show)

instance Functor Tree where
  fmap g Empty = Empty
  fmap g (Node a left right) = Node (g a) (fmap g left) (fmap g right)

{- DESCRIPT BST -}

-- TODO:
type Space = String
incSpace :: Space -> Space
incSpace space = space ++ "  "

-- Pretty print tree
pretty :: (Show a) => Tree a -> Space -> IO ()
pretty Empty _ = putStr ""
pretty (Node x left right) space = do
  putStrLn (space ++ show x)
  pretty left $ incSpace space
  pretty right $ incSpace space

-- Size of tree
size :: Tree a -> Int
size = length . toList

-- Depth of tree
depth :: Tree a -> Int
depth Empty = 0
depth (Node a left right) = max (1 + depth left) (1 + depth right)

-- Check if tree is empty
empty :: Tree a -> Bool
empty Empty = True
empty _ = False

-- Check if x is in tree
contains :: (Ord a) => a -> Tree a -> Bool
contains _ Empty = False
contains x (Node n left right)
  | x == n = True
  | x > n = contains x right
  | x < n = contains x left


{- BST OPERATIONS -}

-- Insert x into tree
insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node n left right)
  | x == n = Node n left right
  | x > n = Node n left (insert x right)
  | x < n = Node n (insert x left) right

-- Delete x from tree
delete :: (Ord a) => a -> Tree a -> Tree a
delete _ Empty = Empty
delete x (Node n left right)
  | x == n = fromList $ toList left ++ toList right
  | x > n = Node n left (delete x right)
  | x < n = Node n (delete x left) right

-- Create list from tree
toList :: Tree a -> [a]
toList Empty = []
toList (Node x left right) = [x] ++ toList left ++ toList right

-- Create tree from list
fromList :: (Ord a) => [a] -> Tree a
fromList [] = Empty
fromList xs = foldl (\tree x -> insert x tree) Empty xs


{- BALANCING BST -}

-- Quicksort
sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (e:xs) = sort [x | x <- xs, x <= e] ++ [e] ++ sort [x | x <- xs, x > e]

-- Middle item of list
middle :: [a] -> [a]
middle [] = []
middle xs = xs !! midPos : []
  where midPos = length xs `div` 2

-- Balanced list from unbalanced list
balancedList :: (Ord a) => [a] -> [a]
balancedList [] = []
balancedList xs = [m] ++ left ++ right
  where 
    m = head . middle $ xs
    left = balancedList [x | x <- xs, x < m]
    right = balancedList [x | x <- xs, x > m]

-- Balanced tree from unbalanced tree
balance :: (Ord a) => Tree a -> Tree a
balance Empty = Empty
balance tree = fromList . balancedList . toList $ tree
