--------------------------------------------------------------------------------
{-| Module      :  MultiSet
    Copyright   :  (c) Daan Leijen 2002
    License     :  BSD-style

    Maintainer  :  wxhaskell-devel@lists.sourceforge.net
    Stability   :  provisional
    Portability :  portable

  An implementation of multi sets on top of the "Map" module. A multi set
  differs from a /bag/ in the sense that it is represented as a map from elements
  to occurrence counts instead of retaining all elements. This means that equality
  on elements should be defined as a /structural/ equality instead of an
  equivalence relation.   If this is not the  case, operations that observe the
  elements, like 'filter' and 'fold',  should be used with care.
-}
---------------------------------------------------------------------------------}
module MultiSet (
            -- * MultiSet type
              MultiSet          -- instance Eq,Show

            -- * Operators
            , (\\)

            -- *Query
            , isEmpty
            , size
            , distinctSize
            , member
            , occur

            , subset
            , properSubset

            -- * Construction
            , empty
            , single
            , insert
            , insertMany
            , delete
            , deleteAll

            -- * Combine
            , union
            , difference
            , intersection
            , unions

            -- * Filter
            , filter
            , partition

            -- * Fold
            , fold
            , foldOccur

            -- * Min\/Max
            , findMin
            , findMax
            , deleteMin
            , deleteMax
            , deleteMinAll
            , deleteMaxAll

            -- * Conversion
            , elems

            -- ** List
            , toList
            , fromList

            -- ** Ordered list
            , toAscList
            , fromAscList
            , fromDistinctAscList

            -- ** Occurrence lists
            , toOccurList
            , toAscOccurList
            , fromOccurList
            , fromAscOccurList

            -- ** Map
            , toMap
            , fromMap
            , fromOccurMap

            -- * Debugging
            , showTree
            , showTreeWith
            , valid
            ) where

import Prelude   hiding  (map,filter)
import qualified Prelude (map,filter)

import qualified Data.Map as M

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 \\

-- | /O(n+m)/. See 'difference'.
(\\) :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
b1 \\ b2 = difference b1 b2

{--------------------------------------------------------------------
  MultiSets are a simple wrapper around Maps, 'Map.Map'
--------------------------------------------------------------------}
-- | A multi set of values @a@.
newtype MultiSet a  = MultiSet (M.Map a Int)

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is the multi set empty?
isEmpty :: MultiSet a -> Bool
isEmpty (MultiSet m)
  = M.null m

-- | /O(1)/. Returns the number of distinct elements in the multi set, ie. (@distinctSize mset == Set.size ('toSet' mset)@).
distinctSize :: MultiSet a -> Int
distinctSize (MultiSet m)
  = M.size m

-- | /O(n)/. The number of elements in the multi set.
size :: MultiSet a -> Int
size b
  = foldOccur (\_x n m -> n+m) 0 b

-- | /O(log n)/. Is the element in the multi set?
member :: Ord a => a -> MultiSet a -> Bool
member x m
  = (occur x m > 0)

-- | /O(log n)/. The number of occurrences of an element in the multi set.
occur :: Ord a => a -> MultiSet a -> Int
occur x (MultiSet m)
  = case M.lookup x m of
      Nothing -> 0
      Just n  -> n

-- | /O(n+m)/. Is this a subset of the multi set?
subset :: Ord a => MultiSet a -> MultiSet a -> Bool
subset (MultiSet m1) (MultiSet m2)
  = M.isSubmapOfBy (<=) m1 m2

-- | /O(n+m)/. Is this a proper subset? (ie. a subset and not equal)
properSubset :: Ord a => MultiSet a -> MultiSet a -> Bool
properSubset b1 b2
  | distinctSize b1 == distinctSize b2 = (subset b1 b2) && (b1 /= b2)
  | distinctSize b1 <  distinctSize b2 = (subset b1 b2)
  | otherwise                      = False

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. Create an empty multi set.
empty :: MultiSet a
empty
  = MultiSet (M.empty)

-- | /O(1)/. Create a singleton multi set.
single :: a -> MultiSet a
single x
  = MultiSet (M.singleton x 0)

{--------------------------------------------------------------------
  Insertion, Deletion
--------------------------------------------------------------------}
-- | /O(log n)/. Insert an element in the multi set.
insert :: Ord a => a -> MultiSet a -> MultiSet a
insert x (MultiSet m)
  = MultiSet (M.insertWith (+) x 1 m)

-- | /O(min(n,W))/. The expression (@insertMany x count mset@)
-- inserts @count@ instances of @x@ in the multi set @mset@.
insertMany ::  Ord a => a -> Int -> MultiSet a -> MultiSet a
insertMany x count (MultiSet m)
  = MultiSet (M.insertWith (+) x count m)

-- | /O(log n)/. Delete a single element.
delete :: Ord a => a -> MultiSet a -> MultiSet a
delete x (MultiSet m)
  = MultiSet (M.updateWithKey f x m)
  where
    f _ n  | n > 0     = Just (n-1)
           | otherwise = Nothing

-- | /O(log n)/. Delete all occurrences of an element.
deleteAll :: Ord a => a -> MultiSet a -> MultiSet a
deleteAll x (MultiSet m)
  = MultiSet (M.delete x m)

{--------------------------------------------------------------------
  Combine
--------------------------------------------------------------------}
-- | /O(n+m)/. Union of two multisets. The union adds the elements together.
--
-- > MultiSet\> union (fromList [1,1,2]) (fromList [1,2,2,3])
-- > {1,1,1,2,2,2,3}
union :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
union (MultiSet t1) (MultiSet t2)
  = MultiSet (M.unionWith (+) t1 t2)

-- | /O(n+m)/. Intersection of two multisets.
--
-- > MultiSet\> intersection (fromList [1,1,2]) (fromList [1,2,2,3])
-- > {1,2}
intersection :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
intersection (MultiSet t1) (MultiSet t2)
  = MultiSet (M.intersectionWith min t1 t2)

-- | /O(n+m)/. Difference between two multisets.
--
-- > MultiSet\> difference (fromList [1,1,2]) (fromList [1,2,2,3])
-- > {1}
difference   :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
difference (MultiSet t1) (MultiSet t2)
  = MultiSet (M.differenceWithKey f t1 t2)
  where
    f _ n m  | n-m > 0   = Just (n-m)
             | otherwise = Nothing

-- | The union of a list of multisets.
unions :: Ord a => [MultiSet a] -> MultiSet a
unions multisets
  = MultiSet (M.unions [m | MultiSet m <- multisets])

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}
-- | /O(n)/. Filter all elements that satisfy some predicate.
filter :: Ord a => (a -> Bool) -> MultiSet a -> MultiSet a
filter p (MultiSet m)
  = MultiSet (M.filterWithKey (\x _ -> p x) m)

-- | /O(n)/. Partition the multi set according to some predicate.
partition :: Ord a => (a -> Bool) -> MultiSet a -> (MultiSet a,MultiSet a)
partition p (MultiSet m)
  = (MultiSet l,MultiSet r)
  where
    (l,r) = M.partitionWithKey (\x _ -> p x) m

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | /O(n)/. Fold over each element in the multi set.
fold :: (a -> b -> b) -> b -> MultiSet a -> b
fold f z (MultiSet m)
  = M.foldrWithKey apply z m
  where
    apply x n z'  | n > 0     = apply x (n-1) (f x z')
                  | otherwise = z'

-- | /O(n)/. Fold over all occurrences of an element at once.
foldOccur :: (a -> Int -> b -> b) -> b -> MultiSet a -> b
foldOccur f z (MultiSet m)
  = M.foldrWithKey f z m

{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}
-- | /O(log n)/. The minimal element of a multi set.
findMin :: MultiSet a -> a
findMin (MultiSet m)
  = fst (M.findMin m)

-- | /O(log n)/. The maximal element of a multi set.
findMax :: MultiSet a -> a
findMax (MultiSet m)
  = fst (M.findMax m)

-- | /O(log n)/. Delete the minimal element.
deleteMin :: MultiSet a -> MultiSet a
deleteMin (MultiSet m)
  = MultiSet (M.updateMin f m)
  where
    f n  | n > 0     = Just (n-1)
         | otherwise = Nothing

-- | /O(log n)/. Delete the maximal element.
deleteMax :: MultiSet a -> MultiSet a
deleteMax (MultiSet m)
  = MultiSet (M.updateMax f m)
  where
    f n  | n > 0     = Just (n-1)
         | otherwise = Nothing

-- | /O(log n)/. Delete all occurrences of the minimal element.
deleteMinAll :: MultiSet a -> MultiSet a
deleteMinAll (MultiSet m)
  = MultiSet (M.deleteMin m)

-- | /O(log n)/. Delete all occurrences of the maximal element.
deleteMaxAll :: MultiSet a -> MultiSet a
deleteMaxAll (MultiSet m)
  = MultiSet (M.deleteMax m)


{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | /O(n)/. The list of elements.
elems :: MultiSet a -> [a]
elems s
  = toList s

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
-- | /O(n)/. Create a list with all elements.
toList :: MultiSet a -> [a]
toList s
  = toAscList s

-- | /O(n)/. Create an ascending list of all elements.
toAscList :: MultiSet a -> [a]
toAscList (MultiSet m)
  = [y | (x,n) <- M.toAscList m, y <- replicate n x]


-- | /O(n*log n)/. Create a multi set from a list of elements.
fromList :: Ord a => [a] -> MultiSet a
fromList xs
  = MultiSet (M.fromListWith (+) [(x,1) | x <- xs])

-- | /O(n)/. Create a multi set from an ascending list in linear time.
fromAscList :: Eq a => [a] -> MultiSet a
fromAscList xs
  = MultiSet (M.fromAscListWith (+) [(x,1) | x <- xs])

-- | /O(n)/. Create a multi set from an ascending list of distinct elements in linear time.
fromDistinctAscList :: [a] -> MultiSet a
fromDistinctAscList xs
  = MultiSet (M.fromDistinctAscList [(x,1) | x <- xs])

-- | /O(n)/. Create a list of element\/occurrence pairs.
toOccurList :: MultiSet a -> [(a,Int)]
toOccurList b
  = toAscOccurList b

-- | /O(n)/. Create an ascending list of element\/occurrence pairs.
toAscOccurList :: MultiSet a -> [(a,Int)]
toAscOccurList (MultiSet m)
  = M.toAscList m

-- | /O(n*log n)/. Create a multi set from a list of element\/occurrence pairs.
fromOccurList :: Ord a => [(a,Int)] -> MultiSet a
fromOccurList xs
  = MultiSet (M.fromListWith (+) (Prelude.filter (\(_, i) -> i > 0) xs))

-- | /O(n)/. Create a multi set from an ascending list of element\/occurrence pairs.
fromAscOccurList :: Ord a => [(a,Int)] -> MultiSet a
fromAscOccurList xs
  = MultiSet (M.fromAscListWith (+) (Prelude.filter (\(_, i) -> i > 0) xs))

{--------------------------------------------------------------------
  Maps
--------------------------------------------------------------------}
-- | /O(1)/. Convert to a 'Map.Map' from elements to number of occurrences.
toMap   :: MultiSet a -> M.Map a Int
toMap (MultiSet m)
  = m

-- | /O(n)/. Convert a 'Map.Map' from elements to occurrences into a multi set.
fromMap :: Ord a => M.Map a Int -> MultiSet a
fromMap m
  = MultiSet (M.filter (>0) m)

-- | /O(1)/. Convert a 'Map.Map' from elements to occurrences into a multi set.
-- Assumes that the 'Map.Map' contains only elements that occur at least once.
fromOccurMap :: M.Map a Int -> MultiSet a
fromOccurMap m
  = MultiSet m

{--------------------------------------------------------------------
  Eq, Ord
--------------------------------------------------------------------}
instance Eq a => Eq (MultiSet a) where
  (MultiSet m1) == (MultiSet m2)  = (m1==m2)

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance Show a => Show (MultiSet a) where
  showsPrec _d b  = showSet (toAscList b)

showSet :: Show a => [a] -> ShowS
showSet []
  = showString "{}"
showSet (x:xs)
  = showChar '{' . shows x . showTail xs
  where
    showTail []       = showChar '}'
    showTail (x':xs') = showChar ',' . shows x' . showTail xs'


{--------------------------------------------------------------------
  Debugging
--------------------------------------------------------------------}
-- | /O(n)/. Show the tree structure that implements the 'MultiSet'. The tree
-- is shown as a compressed and /hanging/.
showTree :: (Show a) => MultiSet a -> String
showTree mset
  = showTreeWith True False mset

-- | /O(n)/. The expression (@showTreeWith hang wide map@) shows
-- the tree that implements the multi set. The tree is shown /hanging/ when @hang@ is @True@
-- and otherwise as a /rotated/ tree. When @wide@ is @True@ an extra wide version
-- is shown.
showTreeWith :: Show a => Bool -> Bool -> MultiSet a -> String
showTreeWith hang wide (MultiSet m)
  = M.showTreeWith (\x n -> show x ++ " (" ++ show n ++ ")") hang wide m


-- | /O(n)/. Is this a valid multi set?
valid :: Ord a => MultiSet a -> Bool
valid (MultiSet m)
  = M.valid m && (M.null (M.filter (<=0) m))
