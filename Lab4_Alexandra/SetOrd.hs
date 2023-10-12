module SetOrd (Set(..),emptySet,isEmpty,inSet,subSet,insertSet,
               deleteSet,powerSet,takeSet,(!!!),list2set,unionSet) 

where

import Data.List (sort) 

{-- Sets implemented as ordered lists without duplicates --} 

newtype Set a = Set [a] deriving (Eq,Ord) -- wraps a set of values around a. deriving (Eq, Ord) - this part specifies that the `set` type 
-- automatically derrives instances of the `Eq` and `Ord` typeclasses. => This means u can compare `Set`
-- values for equality (using `==`) and order them (using `<`, '>') based on the order of the elements in the underlying list

-- -- Define a set of integers
-- mySet :: Set Int
-- mySet = Set [1, 2, 3]

-- -- Compare sets for equality
-- isEqual :: Bool
-- isEqual = mySet == Set [1, 2, 3]

-- -- Compare sets for order (Note: it's based on the order of elements in the list)
-- isOrdered :: Bool
-- isOrdered = Set [1, 2, 3] < Set [2, 3, 4] - True, they compare 1 with 2, 2 with 3, 3 with 4.


instance (Show a) => Show (Set a) where
    showsPrec _ (Set s) str = showSet s str

showSet []     str = showString "{}" str
showSet (x:xs) str = showChar '{' ( shows x ( showl xs str))
     where showl []     str = showChar '}' str
           showl (x:xs) str = showChar ',' (shows x (showl xs str))

emptySet  :: Set a       
emptySet = Set []

isEmpty  :: Set a -> Bool            
isEmpty (Set []) = True
isEmpty _        = False

inSet  :: (Ord a) => a -> Set a -> Bool  
inSet x (Set s) = elem x (takeWhile (<= x) s)

-- first one subset, next one full set
subSet :: (Ord a) => Set a -> Set a -> Bool
subSet (Set []) _       = True  
subSet (Set (x:xs)) set = (inSet x set) && subSet (Set xs) set 

-- newSet :: Set Int
-- newSet = insertSet 4 mySet
insertSet :: (Ord a) => a -> Set a -> Set a 
insertSet x (Set s) = Set (insertList x s) 

-- newList1 = insertList 4 sortedList
insertList x [] = [x]
insertList x ys@(y:ys') = case compare x y of 
                                 GT -> y : insertList x ys' 
                                 EQ -> ys 
                                 _  -> x : ys 

deleteSet :: Ord a => a -> Set a -> Set a 
deleteSet x (Set s) = Set (deleteList x s)

deleteList x [] = []
deleteList x ys@(y:ys') = case compare x y of 
                                 GT -> y : deleteList x ys'
                                 EQ -> ys'
                                 _  -> ys

list2set :: Ord a => [a] -> Set a
list2set [] = Set []
list2set (x:xs) = insertSet x (list2set xs)
-- list2set xs = Set (foldr insertList [] xs)

-- A power set of a set is the set of all possible subsets of that set, including the set itself and the empty set. In other words, it's a set containing all possible combinations of elements from the original set.
-- the order does not mater
powerSet :: Ord a => Set a -> Set (Set a)
powerSet (Set xs) = 
   Set (sort (map (\xs -> (list2set xs)) (powerList xs)))

powerList  :: [a] -> [[a]]
powerList  [] = [[]]
powerList  (x:xs) = (powerList xs) 
                     ++ (map (x:) (powerList xs))

takeSet :: Eq a => Int -> Set a -> Set a
takeSet n (Set xs) = Set (take n xs) 

infixl 9 !!!

(!!!) :: Eq a => Set a -> Int -> a 
(Set xs) !!! n = xs !! n

unionSet :: (Ord a) => Set a -> Set a -> Set a 
unionSet (Set [])     set2  =  set2
unionSet (Set (x:xs)) set2  = 
   insertSet x (unionSet (Set xs) set2)

