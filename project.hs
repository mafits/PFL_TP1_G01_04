import Data.List
import Data.Ord
import Data.Char
import Data.Function
import Data.Monoid (mappend)

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

-- soma dois monómios (com variáveis e expoentes iguais)
joinMon :: ([(String, Int)], [Int]) -> ([(String, Int)], [Int]) -> ([(String, Int)], [Int])
joinMon x y = ( fst y, (snd y) ++ (snd x) )

-- soma vários monómios (com variáveis e expoentes iguais)
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

removeNull :: [(String, Int)] -> [(String, Int)]
removeNull [] = []
removeNull (x:xs) = if snd x == 0 then removeNull xs else x : removeNull xs

-- apagar e substituir por map
removeVar :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
removeVar xs = map removeNullExp xs


removeNullExp :: ([(String, Int)], [Int]) -> ([(String, Int)], [Int])
removeNullExp a = addEqualVar(removeNull(fst a), snd a)

myPredicate (a1, a2) (b1, b2) = compare a1 b1 `mappend` compare a2 b2

sortMon :: Ord a => Ord b => [(a, b)] -> [(a, b)]
sortMon = sortBy (myPredicate) 

findMax:: [([Char] ,Int)]-> Int
findMax [] = 0
findMax [a] = snd a
findMax (x:xs) = if( (snd x)>= (snd (head xs))) then findMax ([x]++(tail xs))
            else findMax xs

sortPol z = reverse(sortOn (\(x,y)->(findMax  x)) z) 

-- 1.
-- Normalize a polynomial
normal :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
normal a = sortPol (removeVar (removeMon (sumMon (joinPoly a))))

stringifyNormal ::  [([(String, Int)], [Int])]  -> String
stringifyNormal y = stringify  ( map (\x -> (sortMon (fst x), snd x)) (normal y))

-- 2.
-- sum two polynomials
sumPoly :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])] -> String
sumPoly a b = stringify (normal (a ++ b))

-- sum multiple polynomials in a list
sumPolyList :: [[([(String, Int)], [Int])]] -> String
sumPolyList a = stringify (normal (foldl (++) [] a))

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

-- substituir por map (apagar esta função)
addEqualVar :: ([(String, Int)], [Int]) -> ([(String, Int)], [Int])
addEqualVar a = (equalVar (fst a), snd a)

-- distribuir um mon pelo polinomio
mulMonPoly :: ([(String, Int)], [Int]) -> [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
mulMonPoly x [] = []
mulMonPoly x (y:ys) = [addEqualVar(mulMon x y)] ++ mulMonPoly x ys

-- multiplicação
mulPoly :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
mulPoly [] y = []
mulPoly (x:xs) y = mulMonPoly x y ++ mulPoly xs y

-- print multiplicação
mult :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])] -> String
mult a b =  stringify (normal (mulPoly a b))

-- passar variaveis de monomio para string (x^2)
stringifyVar :: [(String, Int)] -> String
stringifyVar a = foldl (++) "" (map (\x -> if ((snd x) /= 1) then (fst x ++ "^" ++ (show (snd x))) else (fst x)) (sortMon a))

-- passar monómio para string
stringifyMon :: ([(String, Int)], [Int]) -> String 
stringifyMon ([], a) = show (a !! 0)
stringifyMon a = show (abs ((snd a) !! 0)) ++ "*" ++ stringifyVar (fst a)

-- passar polinómio para string
stringifyPol :: [([(String, Int)], [Int])] -> String
stringifyPol a = foldl (++) "" (map (\x -> if (((snd x) !! 0) > 0 ) then (" + " ++ (stringifyMon x)) else (" - " ++ (stringifyMon x))) a)

-- adicionar as operações entre os monómios
stringify :: [([(String, Int)], [Int])] -> String
stringify (x:xs) = if ((snd x) !! 0 > 0) then (stringifyMon x) ++ (stringifyPol xs) else "- " ++ (stringifyMon x) ++ (stringifyPol xs)

-- 4. derivação
-- mon não tem String -> 0
-- mon tem String e mais nada -> exp -1; coef * exp
-- mon tem String e mais -> derivar String * resto 

stringIn :: [(String, Int)] -> String -> Bool
stringIn [] b = False
stringIn (x:xs) b = if fst x == b then True else stringIn xs b

-- derivação com string
derive :: [(String, Int)] -> [Int] -> ([(String, Int)], [Int])
derive a [b] = ([(fst(a!!0), snd(a!!0)-1)], [b*snd(a!!0)])

filterVar :: [(String, Int)] -> String -> [(String, Int)]
filterVar [] b = []
filterVar (x:xs) b = if fst x == b then [x] else filterVar xs b

notFilterVar :: [(String, Int)] -> String -> [(String, Int)]
notFilterVar [] b = []
notFilterVar (x:xs) b = if fst x /= b then x : notFilterVar xs b else notFilterVar xs b

concatVar :: ([(String, Int)], [Int]) -> [(String, Int)] -> ([(String, Int)], [Int])
concatVar a b = (fst a ++ b, snd a)

-- tem var = String, fazer derivação da multiplicação
deriveMul :: ([(String, Int)], [Int]) -> String -> ([(String, Int)], [Int])
deriveMul a b = concatVar(derive (filterVar (fst a) b) (snd a)) (notFilterVar (fst a) b)

deriveMon :: ([(String, Int)], [Int]) -> String -> ([(String, Int)], [Int])
deriveMon a b = if stringIn (fst a) b then deriveMul a b else ([],[0])

derivePol :: [([(String, Int)], [Int])] -> String -> String
derivePol a b = stringify (normal (map (\x -> deriveMon x b) (normal a)))


--[([("x",1),("y",3)],[3,5]),([("x",7)],[9]),([("y",3),("x",1)],[7])]

--([("x",1),("y",3)],[3,5])

--[("x",1),("y",3)]

-- [([("x",9),("y",3)],[3,5]),([("x",7)],[9]),([("y",3),("x",1)],[7])]

--[([("x",1),("y",3)],[3,5]),([("x",7)],[9]),([("y",3),("x",1)],[7])]
