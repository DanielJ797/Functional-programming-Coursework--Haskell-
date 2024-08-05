
lsplit :: (Eq a) => [a] -> a -> [Int]

lsplit a b = checkPartition a b 0



checkPartition :: (Eq a) => [a] -> a -> Int -> [Int]

checkPartition x y z
 | length x == 0 = [z]
 | (head x) == y && z == 0 = checkPartition (tail x) y 0
 | (head x) == y = [z] ++ checkPartition (tail x) y 0
 | otherwise = checkPartition (tail x) y (z+1)


