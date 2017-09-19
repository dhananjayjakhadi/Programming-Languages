import Data.List
import Debug.Trace

-- Problem 1
-- Return pair containing roots of quadratic equation a*x**2 + b*x + c.
-- The first element in the returned pair should use the positive 
-- square-root of the discriminant, the second element should use the 
-- negative square-root of the discriminant.  Need not handle complex
quadraticRoots :: Floating t => t -> t -> t -> (t, t)
quadraticRoots a b c =( ((-b)+sqrt(b*b-4*a*c))/(2*a), ((-b)-sqrt(b*b-4*a*c))/(2*a) )

-- Problem 2
-- Return infinite list containing [z, f(z), f(f(z)), f(f(f(z))), ...]
-- May use recursion.
iterateFunction :: (a -> a) -> a -> [a]
iterateFunction f z = [z] ++ iterateFunction f (f z)

-- Problem 3
-- Using iterateFunction return infinite list containing 
-- multiples of n by all the non-negative integers.
-- May NOT use recursion.
multiples n = iterateFunction (\x -> x+n) 0

-- Problem 4
-- Use iterateFunction to return an infinite list containing list 
-- of hailstone numbers starting with n.
-- Specifically, if i is a hailstone number, and i is even, then
-- the next hailstone number is i divided by 2; if i is a hailstone
-- number and i is odd, then the next hailstone number is 3*i + 1.
-- May NOT use recursion.
hailstones :: Integral a => a -> [a]
hailstones n = iterateFunction (\ n -> case () of
    _ | (n `mod` 2 == 0) -> n `div` 2 
    _ | otherwise -> 3 * n + 1) n

-- Problem 5
-- Return length of hailstone sequence starting with n terminating
-- at the first 1.
-- May NOT use recursion.  Can use elemIndex from Data.List
hailstonesLen :: Integral a => a -> Int
hailstonesLen n = 
       sum ([n] ++ takeWhile (/= 1) (iterateFunction oddF n))
                where
                sum l = foldl (\n _ -> n + 1) 0 l
                oddF x
                        | even x = quot x 2
                        | odd x = (3 * x) + 1

-- Problem 6
-- Given a list of numbers, return sum of the absolute difference
-- between consecutive elements of the list.
-- May NOT use recursion.
sumAbsDiffs :: Num a => [a] -> a
sumAbsDiffs numberList = 
        if length numberList == 0
             then 0
                 else foldl (+) 0 (map (\y -> abs(y)) (zipWith (-) (tail numberList) (init numberList)))

-- Problem 7
-- The x-y coordinate of a point is represented using the pair (x, y).
-- Return the list containing the distance of each point in list
-- points from point pt.
-- May NOT use recursion.
distances :: Floating b => (b, b) -> [(b, b)] -> [b]
distances pt points = map ((\(x1,y1) (x2,y2) -> sqrt( (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) )) pt) points

-- Problem 8
-- Given a list of coordinate pairs representing points, return the
-- sum of the lengths of all line segments between successive 
-- adjacent points.
-- May NOT use recursion.
sumLengths :: Floating a => [(a, a)] -> a
sumLengths pointsList = 	
          if length pointsList == 0         
             then 0
                else foldl (+) 0 (zipWith dist (tail pointsList) (init pointsList))
                    where
                      dist (x2, y2) (x1, y1) = sqrt ((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))

-- Problem 9
-- Given a string s and char c, return list of indexes in s where c
-- occurs
occurrences s c = elemIndices c s

-- A tree of some type t is either a Leaf containing a value of type t,
-- or it is an internal node (with constructor Tree) with some left
-- sub-tree, a value of type t and a right	 sub-tree.
data Tree t = Leaf t
            | Tree (Tree t) t (Tree t)

-- Problem 10
-- Fold tree to a single value.  If tree is a Tree node, then it's
-- folded value is the result of applying ternary treeFn to the result
-- of folding the left sub-tree, the value stored in the Tree node and
-- the result of folding the right sub-tree; if it is a Leaf node,
-- then the result of the fold is the result of applying the unary
-- leafFn to the value stored within the Leaf node.
-- May use recursion.
foldTree :: (t1 -> t -> t1 -> t1) -> (t -> t1) -> Tree t -> t1
foldTree treeFn leafFn tree = 
          case tree of
          Tree left parent right -> treeFn (foldTree treeFn leafFn left)  parent  (foldTree treeFn leafFn right)
          Leaf l -> leafFn l

-- Problem 11
-- Return list containing flattening of tree.  The elements of the
-- list correspond to the elements stored in the tree ordered as per 
-- an in-order traversal of the tree. Must be implemented using foldTree.
-- May NOT use recursion.
flattenTree :: Tree a -> [a]
flattenTree tree =  foldTree (\d e f -> d ++ [e] ++ f) (\d -> [d]) tree 

-- Problem 12	
-- Given tree of type (Tree [t]) return list which is concatenation
-- of all lists in tree.
-- Must be implemented using flattenTree.
-- May NOT use recursion.
catenateTreeLists :: Tree [a] -> [a]
catenateTreeLists tree = foldl (\p q -> p ++ q ) [] (flattenTree tree) 
                          
