{-

generate a list and count of all possible 3 letter words contained within an English dictionary.
If you want to show off your optimization skills how about generating all possible words for length 1 up to 19
-}
import Data.List 
import Data.Char

-- Dictionary for Mac
dict = "/usr/share/dict/words"

inputString = "optimizationmatters"

-- * use this if upper case is treated as same as lower case
-- inputString = inputString ++ map toUpper inputString

uniqueAppearance = [c | (c:_) <- group $ sort inputString]
neverAppeared = ['A'..'z'] \\ uniqueAppearance

main :: IO ()
main = do 
  s <- readFile dict
  let ws = words s
  -- words which are noLonger than 19 and notContaining neverAppeared
  let ws' = noLonger 19 $ foldr notContain ws neverAppeared
  -- print $ length ws' -- around 7357
  print $ filter (flip matching inputString) ws'

-- this could be nice pre-filter if inputString containing more than 12 diff alphabets
notStartWith c ws = filter ((/= c) . head) ws

notContain c ws   = filter (not . elem c) ws

lengthEq i ws    = filter ((== i) . length) ws 
noLonger i ws    = filter ((<= i) . length) ws 


matching :: String -> String -> Bool
matching _ []     = False
matching [] _     = True
matching (x:xs) s = if x `elem` s 
                    then matching xs (s \\ [x])
                    else False