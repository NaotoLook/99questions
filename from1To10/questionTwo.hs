myButLast :: [a] -> a
myButLast xs = head $ reverse $ init xs
