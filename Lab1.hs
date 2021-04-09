import Data.List
import System.IO

test1 :: [Int]
test2 :: [Int]
test1 = [x*(-1)^x | x <- [1..100]]
test2 = [24,-11,-34,42,-24,7,-19,21]
--Returns the sum of all elements in the input list
listSum :: [Int]->Int
listSum [] = 0
listSum (x:xs) = x + listSum xs

--Returns the index pair which results the minimum sum of 2 pairs in the array
minOfTwoArrays:: (Int,Int)->(Int,Int)->[Int]->(Int,Int)
minOfTwoArrays a b array
    | listSum ( drop (fst a) (take ( (snd a) + 1) array) ) < listSum ( drop (fst b) (take ( (snd b) + 1) array) )     = a
    | otherwise                 = b

--Removes one tuple of a tuples list
removeItem :: (Int,Int)->[(Int,Int)]->[(Int,Int)]
removeItem _ [] = []
removeItem x (y:ys) |x==y       =removeItem x ys
                    |otherwise  =y : removeItem x ys

--Returns all the possible subarrays in form of tuple pairs of index i and j
subArrays :: Int->Int->[Int]->[(Int,Int)] --subArrays(i,j, array) -> all subArrays indexes
subArrays i j array 
    | array==[]                                             = []
    | j < (length array) - 1                                = [(i, j)] ++ subArrays i (j+1) array
    | j >= (length array) - 1 && i <= (length array) - 1    = [(i, j) ] ++ subArrays (i+1) (i+1) array
    |otherwise                                              = []

--Find the tuple pair which returns the smallest sum in the array
findMinSubArray :: [(Int,Int)]->[Int]->(Int,Int) -- findMinSubArray(pairs of tuples (i,j), array) -> the tuples which return smallest sum
findMinSubArray [] array = (0,0)
findMinSubArray (x:xs) array = minOfTwoArrays x (findMinSubArray xs array) array

formatConversion :: [(Int,Int)]->[Int]->[((Int,Int), [Int])]
formatConversion [] array = []
formatConversion (x:xs) array = [(x,drop (fst x) (take ((snd x) + 1) array) )] ++ formatConversion xs array

--Returns the k smallest tuple pairs which returns the smallest sums
getKSmallestIndexPairs:: [(Int,Int)]->[Int]->Int->[(Int,Int)] --kSmallest(all possibles subarrays index, array, k) -> List of tuples: (size, sublist, i, j)
getKSmallestIndexPairs subArrs array k
    |k>0        = [findMinSubArray subArrs array] ++ getKSmallestIndexPairs(removeItem (findMinSubArray subArrs array) subArrs) array (k-1)
    |otherwise  = []

--Trying to make the getKSmallestIndexPairscall prettier, calls kSmallest
kSmallestSubList :: [Int]->Int->[((Int,Int),[Int])]
kSmallestSubList array k = formatConversion (getKSmallestIndexPairs(subArrays 0 0 array) array k) array
