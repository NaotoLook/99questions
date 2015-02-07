--Problem 1
-- Find the last element of a list.
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

--Problem 2
-- Find the last but one element of a list.
myButLast :: [a] -> a
myButLast xs = head $ reverse $ init xs

--Problem 3
-- Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a]-> Int -> a
elementAt elements n
    | n >= length elements = error "No element"
    | n == 1 = head elements
    | n >  1 = elementAt (tail elements) (n-1) 

--Problem 4
-- Find the number of elements of a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = myLength xs + 1

--Problem 5
-- Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = last xs : (myReverse $ init xs) -- myReverse xs ++ [x] 

--Problem 6
-- Find out whether a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool 
isPalindrome xs = xs == (reverse xs)

--Problem 7 
-- Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

--Problem 8
-- Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs)
    |        xs == [] = []
    |  x == (head xs) = compress xs
    |       otherwise = x : compress xs

--Problem 9
-- Pack consecutive duplicates of list elements into sublists. 
-- If a list contains repeated elements they should be placed in separate sublists.
-- Example :
-- Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x :(takeWhile (== x) xs)) : pack  (dropWhile (== x) xs)

--Problem 10
-- Run-length encoding of a list.
-- Use the result of problem P09 to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
-- Example : 
-- encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode :: (Eq a) => [a] -> [(Int,a)]
encode [] = []
encode xs = 
    let
        list = pack xs
    in
      map (\xs -> (myLength xs,head xs)) list
    
--Problem 11
-- Modified run-length encoding.
data ListType a = Single a | Multiple Int a
                deriving (Show)

encodeModified :: (Eq a) => [a] -> [ListType a] 
encodeModified  = map encodeHelper . encode 
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x

--Problem 12
-- Decode a run-length encoded list.
decodeModified :: [ListType a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decodeHelper x ++ decodeModified xs 
    where
      decodeHelper (Single x) = replicate 1 x
      decodeHelper (Multiple i x) = replicate i x

--Problem 14
-- Duplicate the elements of a list.
-- Example : 
-- > dupli [1, 2, 3]
-- [1,1,2,2,3,3]
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs 

--Problem 15
-- Replicate the elements of a list a given number of times.
-- Example :
-- > repli "abc" 3
-- "aaabbbccc"
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n
    | n <= 0    = []
    | otherwise = replicate n x ++ repli xs n 

--Problem 16
-- Drop every N'th element from a list.
-- Example :
-- > dropEvery "abcdefghik" 3
-- "abdeghk"
dropEvery :: [a] -> Int -> [a]
dropEvery [] _  = []
dropEvery xs n
    | n < 0     = error "error"
    | n == 0    = xs
    | n > length xs = xs
    | otherwise = (init $ take n xs) ++ dropEvery (dropEvery' xs n) n
    where
      dropEvery' [] _ = []
      dropEvery' xs n
          | n == 0    = xs 
          | otherwise = dropEvery' (tail xs) (n-1) 

--Problem 17
-- Split a list into two parts; the length of the first part is given.
-- Example : 
-- Main> split "abcdefghik" 3
-- ("abc", "defghik")
split :: [a] -> Int -> ([a],[a])
split [] _ = ([],[])
split all@(x:xs) n
    | n > 0     = (x:y,ys)
    | otherwise = ([],all)
    where 
      (y,ys) = split xs (n-1)

--Problem 18
-- Extract a slice from a list.
-- Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"
slice :: [a] -> Int -> Int -> [a]
slice (x:xs) sPosition ePosition 
    | sPosition > 1 && ePosition > 1 = slice xs (sPosition-1) (ePosition-1)
    | sPosition == 1 && ePosition > 1 = x : slice xs sPosition (ePosition-1)
    | sPosition == 1 && ePosition == 1 = [x]
