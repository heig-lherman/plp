{-
  Set.hs - Functional integer sets in haskell
  Authors: Loïc Herman, Massimo Stefani
-}
module Set where
import Data.List (intercalate)

-- | A set of integers.
type Set = Int -> Bool

-- | Show instance for our functional sets.
-- Note: requires the TypeSynonymInstances language extension on Haskel2010
instance Show Set where
  show set = "{" ++ intercalate ", " (map show (toList set)) ++ "}"

-- | The upper bound of a set.
bound :: Int
bound = 1000

-- | The empty set.
empty :: Set
empty _ = False

-- | The singleton set.
singleton :: Int -> Set
singleton x y = x == y

-- | Insert an element into a set.
insert :: Int -> Set -> Set
insert x set y = x == y || set y

-- | Delete an element from a set.
delete :: Int -> Set -> Set
delete x set y = x /= y && set y

-- | Pop the smallest element from a set.
pop :: Set -> (Maybe Int, Set)
pop set = case findMin set of
  Nothing -> (Nothing, set)
  Just x -> (Just x, delete x set)

-- | Check if an element is in a set.
member :: Set -> Int -> Bool
member set = set

-- | The size of a set.
size :: Set -> Int
size set = size' set (-bound) 0
  where
    size' set x counter
      | x == bound = counter
      | set x = size' set (x+1) (counter+1)
      | otherwise = size' set (x+1) counter

-- | The union of two sets.
union :: Set -> Set -> Set
union set1 set2 x = set1 x || set2 x

-- | The intersection of two sets.
intersection :: Set -> Set -> Set
intersection set1 set2 x = set1 x && set2 x

-- | The difference of two sets.
difference :: Set -> Set -> Set
difference set1 set2 x = set1 x && not (set2 x)

-- | Check if a set is a subset of another set.
subset :: Set -> Set -> Bool
subset = every

-- | Check if two sets are disjoint.
disjoint :: Set -> Set -> Bool
disjoint set1 set2 = none set1 set2

-- | Apply a function to a set.
apply :: Set -> (Int -> Int) -> Set
apply set f = apply' (-bound)
  where
    apply' x y
      | x > bound = False
      | set x && f x == y = True
      | otherwise = apply' (x + 1) y

-- | Select elements from a set.
select :: Set -> (Int -> Bool) -> Set
select set p x = set x && p x

-- | Reduce a set to a single value.
reduce :: Set -> (Int -> Int -> Int) -> Int -> Int
reduce = reduce' (-bound)
  where
    reduce' x set f acc
      | x > bound = acc
      | set x = reduce' (x+1) set f (f x acc)
      | otherwise = reduce' (x+1) set f acc

-- | Check if a predicate holds for every element in a set.
every :: Set -> (Int -> Bool) -> Bool
every = every' (-bound)
  where
    every' x set p
      | set x && not (p x) = False
      | x == bound = True
      | otherwise = every' (x+1) set p

-- | Check if a predicate holds for some element in a set.
some :: Set -> (Int -> Bool) -> Bool
some = some' (-bound)
  where
    some' x set p
      | set x && p x = True
      | x == bound = False
      | otherwise = some' (x+1) set p

-- | Check if a predicate holds for no element in a set.
none :: Set -> (Int -> Bool) -> Bool
none set p = not (some set p)

-- | Find the minimum element in a set.
findMin :: Set -> Maybe Int
findMin set = findMin' (-bound)
  where
    findMin' x
      | x > bound = Nothing
      | set x = Just x
      | otherwise = findMin' (x + 1)

-- | Find the maximum element in a set.
findMax :: Set -> Maybe Int
findMax set = findMax' bound
  where
    findMax' x
      | x < -bound = Nothing
      | set x = Just x
      | otherwise = findMax' (x - 1)

-- | Partition a set into two sets.
partition :: Set -> (Int -> Bool) -> (Set, Set)
partition set p = (select set p, select set (not . p))

-- | Split a set into two sets.
split :: Set -> Int -> (Set, Set)
split set x =  (select set (<x), select set (>=x))

-- | Create a set from a list of elements.
fromList :: [Int] -> Set
fromList xs = (`elem` xs)

-- | Create a list of elements from a set.
toList :: Set -> [Int]
toList set = [x | x <- [-bound..bound], set x]
