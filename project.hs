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


-- testa se uma variável + expoente está presente numa lista de variáveis + expoentes
exists:: (String, Int) -> [(String, Int)] -> Bool
exists a [] = False
exists a (b:xs) | a == b = True
                | otherwise = exists a xs

-- testa se as variáveis e expoentes respetivos de 2 pol são iguais
isEqual:: [(String, Int)] -> [(String, Int)] -> Bool
isEqual [] b = True
isEqual (a:xs) b | exists a b = isEqual xs b
                 | otherwise = False

delete :: Eq a => a -> [a] -> [a]
delete deleted xs = [ x | x <- xs, x /= deleted ]

-- concatena os coeficientes de 2 mon com var + exp iguais
joinMon :: ([(String, Int)], [Int]) -> ([(String, Int)], [Int]) -> ([(String, Int)], [Int])
joinMon x y = ( fst y, (snd y) ++ (snd x) )

-- concatena os coeficientes de todos os mon com var + exp iguais
joinMonList :: [([(String, Int)], [Int])]  -> [([(String, Int)], [Int])]
joinMonList (x:xs) = [foldl (\z y -> joinMon z y) x xs]

-- junta os monómios que têm var + exp iguais
joinPoly :: [([(String, Int)], [Int])]  -> [([(String, Int)], [Int])]
joinPoly [] = []
joinPoly (x:xs) = (joinMonList ([x] ++ (filter (\y -> isEqual (fst x) (fst y)) xs))) ++ joinPoly (filter (\y -> not (isEqual (fst x) (fst y))) xs)

-- soma os coeficientes (da mesma lista)
sumMon :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
sumMon a = map (\x -> (fst x, [sum (snd x)])) a

-- remover monómios com coeficiente = 0
removeMon :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
removeMon a = filter (\x -> (not (((snd x) !! 0) ==0) && ( not ((snd x)==[])))) a

-- 1.
normal :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
normal a = removeMon (sumMon (joinPoly a))

-- 2.
sumPoly :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
sumPoly a b = normal (a ++ b)

-- 3.
-- multiplicar 2 mon
mulMon :: ([(String, Int)], [Int]) -> ([(String, Int)], [Int]) -> ([(String, Int)], [Int])
mulMon x y = (fst x ++ fst y, [(snd x)!!0 * (snd y)!!0])

mydel :: (String, Int) -> [(String, Int)] -> [(String, Int)]
mydel a [] = []
mydel a (x:xs) = if fst(a) == fst(x) then mydel a xs else x : mydel a xs

addExp :: (String, Int) -> [(String, Int)] -> (String, Int)
addExp a [] = a
addExp a (x:xs) = if (fst(a) == fst(x)) then addExp (fst(a), snd(a)+snd(x)) xs else addExp a xs

equalVar :: [(String, Int)] -> [(String, Int)]
equalVar [] = []
equalVar (x:xs) = addExp x xs : equalVar(mydel (addExp x xs) xs)

addEqualVar :: ([(String, Int)], [Int]) -> ([(String, Int)], [Int])
addEqualVar a = (equalVar (fst a), snd a)

-- distribuir um mon pelo polinomio
mulMonPoly :: ([(String, Int)], [Int]) -> [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
mulMonPoly x [] = []
mulMonPoly x (y:ys) = [addEqualVar(mulMon x y)] ++ mulMonPoly x ys

-- multiplicação
mulPoly :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
mulPoly [] y = []
mulPoly (x:xs) y = normal(mulMonPoly x y ++ mulPoly xs y)



-- normal [([("x",2),("y",3)],[2]), ([("y",3)],[3]),([("y",3),("x",2)],[5]), ([("y",2)],[5]), ([("z",2)],[])]


