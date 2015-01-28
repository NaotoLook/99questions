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
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x :(takeWhile (== x) xs)) : pack  (dropWhile (== x) xs)
