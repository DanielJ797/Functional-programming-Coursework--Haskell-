
compatibility :: String -> String -> String

compatibility a b = (finalString a b) ++ " and " ++ (finalString b a)


checkLetter :: Char -> String -> Bool

checkLetter x y
 | length y == 0 = False
 | x == head y = True 
 | otherwise = checkLetter x (tail y) 



checkWord :: String -> String -> String

checkWord a b
 | length a == 0 = ""
 | checkLetter (head a) b == True = "*" ++ (checkWord (tail a) b) 
 | checkLetter (head a) b == False = [(head a)] ++ (checkWord (tail a) b)  



applyRelationship :: String -> String -> String -> String

applyRelationship a b c
 | length (checkWord a b) == 0 = ""
 | head (checkWord a b) == '*' = applyRelationship (tail a) b c
 | otherwise = [head (getRelationship c)] ++ (applyRelationship (tail a) b (getRelationship c)) 



getRelationship :: String -> String

getRelationship inputList = (tail inputList ++ [(head inputList)])



stateRelationship :: String -> String

stateRelationship x
 | last x == 'l' = "loves"
 | last x == 'p' = "is physical with"
 | last x == 'h' = "hates"
 | last x == 'i' = "is indifferent to"



finalString :: String -> String -> String

finalString a b = a ++ " " ++ stateRelationship (applyRelationship a b "ilph") ++ " "++ b