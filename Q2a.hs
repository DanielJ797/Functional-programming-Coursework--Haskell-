steps :: Int -> Int -> Int -> String

steps m n p = init (getRecursion m n p ((2*p)-1))



getRow :: Int -> String

getRow n
 | n > 0 = '*':getRow (n-1)
 | otherwise = ""



getColumn :: Int -> String -> String

getColumn m string
 | m > 0 = getColumn (m-1) string ++ string ++ "\n"
 | otherwise = ""



getInverse :: Int -> [Int]

getInverse p = [1..p] ++ reverse[1..p]



getInverseElement :: [Int] -> Int -> Int

getInverseElement list element = list!!element



getRecursion :: Int -> Int -> Int -> Int -> String

getRecursion m n p i
 | i >= 0 = getColumn m (getRow (n * (getInverseElement (getInverse p) i))) ++ getRecursion m n p (i-1) 
 | otherwise = ""





 
