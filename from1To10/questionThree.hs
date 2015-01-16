elementAt :: [a]-> Int -> a
elementAt elements n
    | n >= length elements = error "No element"
    | n == 1 = head elements
    | n >  1 = elementAt (tail elements) (n-1) 
