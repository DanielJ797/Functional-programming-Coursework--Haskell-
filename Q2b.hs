
flagpattern :: Int -> Int-> String

flagpattern a b = init (numberofFlags a b)



numberofFlags :: Int -> Int -> String

numberofFlags a b
 | b > 0 = (border a ++ "\n" ++ (flagBuild ((a-1) `div` 2) ((a-1) `div` 2) a) ++ border a ++ "\n") ++ numberofFlags a (b-1)
 | otherwise = ""



border :: Int -> String

border x
 | x > 0 = '*':border (x-1)
 | otherwise = ""



rowNumber :: Int -> Int-> String

rowNumber a b
 | (a-b) == 0 = '*':rowNumber a (b-1)
 | (a-b) < a = ' ':rowNumber a (b-1)
 | otherwise = ""



rowBuild :: Int -> String -> String

rowBuild a b 
 | a `mod` 2 == 0 = b ++ (reverse (b))
 | otherwise = b ++ tail (reverse (b))



flagBuild :: Int -> Int-> Int-> String

flagBuild a b c
 | a == 0 = flagBuild (a-2) b c
 | (abs a) <= b = "*" ++ rowBuild c (rowNumber (abs a) b) ++ "*\n" ++ flagBuild (a-1) b c
 | otherwise = ""







