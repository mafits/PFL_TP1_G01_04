import Data.List
import Data.Ord
import Data.Char

-- show This gets rid of all the zeroes at the end of the representation.
toProper [] = []
toProper p = if (last p /= 0) -- this means there is nothing to do.
             then p 
             else toProper $ init p 

toStringFinal :: [String]-> String
toStringFinal (x:xs) = x ++ " + " ++ toStringFinal xs
toStringFinal [] = ""

toString :: [Int] -> [String]
toString a = map show a

-- alterar to map
toInt :: [String] -> [Int]
toInt l =  map (\x -> read x::Int) l

append :: Int -> [Int] -> [Int]
append a [] = [a]
append a (x:xs) = x : append a xs

{-}
normal :: [(String,[Int])] -> [(String,[Int])]
normal a = filter (\x -> (snd x !! 0) 0) (map (\x -> (fst x, [sum (snd x)])) a) -}

-- concatenar
-- somar
-- tirar os zeros



exists:: (String, Int) -> [(String, Int)] -> Bool
exists a [] = False
exists a (b:xs) | a == b = True
                | otherwise = exists a xs

isEqual:: [(String, Int)] -> [(String, Int)] -> Bool
isEqual [] b = True
isEqual (a:xs) b | exists a b = isEqual xs b
                 | otherwise = False

delete :: Eq a => a -> [a] -> [a]
delete deleted xs = [ x | x <- xs, x /= deleted ]

joinMon :: ([(String, Int)], [Int]) -> ([(String, Int)], [Int]) -> ([(String, Int)], [Int])
joinMon x y = ( fst y, (snd y) ++ (snd x) )

joinMonList :: [([(String, Int)], [Int])]  -> [([(String, Int)], [Int])]
joinMonList (x:xs) = [foldl (\z y -> joinMon z y) x xs]

joinPoly :: [([(String, Int)], [Int])]  -> [([(String, Int)], [Int])]
joinPoly [] = []
joinPoly (x:xs) = (joinMonList ([x] ++ (filter (\y -> isEqual (fst x) (fst y)) xs))) ++ joinPoly (filter (\y -> not (isEqual (fst x) (fst y))) xs)

sumMon :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
sumMon a = map (\x -> (fst x, [sum (snd x)])) a

removeMon :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
removeMon a = filter (\x -> (not (((snd x) !! 0) ==0) && ( not ((snd x)==[])))) a

-- 1.
normal :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
normal a = removeMon (sumMon (joinPoly a))

-- 2.
sumPoly :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
sumPoly a b = normal (a ++ b)





-- normal [([("x",2),("y",3)],[2]), ([("y",3)],[3]),([("y",3),("x",2)],[5]), ([("y",2)],[5]), ([("z",2)],[])]


