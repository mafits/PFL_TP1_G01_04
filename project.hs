import Data.List
import Data.Ord
import Data.Char
import Data.Function
import Data.Monoid (mappend)


----   PARSING: de string para polinómio

-- remover os espaços das strings de input
removeSpace :: String -> String
removeSpace = filter (not . isSpace)

-- retorna verdadeiro no caso de a componente do monómio ter uma variável (uma letra)
hasLetter :: String -> Bool
hasLetter [] = False
hasLetter (x:xs) = if isLetter x then True else hasLetter xs

-- transformar a incógnita na representação interna
parseVar :: String -> (String, Int)
parseVar a | length a == 1 = (a, 1)
           | otherwise = (takeWhile (/= '^') a, read (drop 2 a) :: Int)

-- transformar o coeficiente na representação interna
parseCoef :: String -> Int
parseCoef a = read a :: Int

-- dividir um monómio numa lista de componentes (coeficiente e incógnitas)
divideInComp :: String -> [String]
divideInComp [] = []
divideInComp ('*':xs) = divideComp(xs)
divideInComp a | (head (takeWhile (/= '*') a) == '-') && isLetter (takeWhile (/= '*') a !! 1) = "-1" : tail (takeWhile (/= '*') a) : divideInComp (dropWhile (/= '*') a)
               | otherwise = takeWhile (/= '*') a : divideInComp (dropWhile (/= '*') a)

-- parse de cada monómio
parseMon :: String -> ([(String, Int)], [Int])
parseMon (x:xs) | length (filter (not . hasLetter) (divideInComp (x:xs))) == 0 = (map parseVar (filter hasLetter (divideInComp (x:xs))), [1])
                | otherwise = (map parseVar (filter hasLetter (divideInComp (x:xs))), map parseCoef (filter (not . hasLetter) (divideInComp (x:xs))))

-- parse do polinómio
parsePoly :: String -> [([(String, Int)], [Int])]
parsePoly [] = []
parsePoly ('+':xs) = parsePoly xs
parsePoly (x:xs) | x == '-' = parseMon (x : takeWhile (\x -> (x /= '+') && (x /= '-')) xs) : parsePoly (dropWhile (\x -> (x /= '+') && (x /= '-')) xs)
                 | otherwise = parseMon (takeWhile (\x -> (x /= '+') && (x /= '-')) (x:xs)) : parsePoly (dropWhile (\x -> (x /= '+') && (x /= '-')) (x:xs))

-- parse do polinómio sem espaços
parse :: String -> [([(String, Int)], [Int])]
parse a = parsePoly(removeSpace a)


----   FUNÇÕES DE ORDENAÇÃO

--Função auxiliar : encontra o maior expoente de entre todas as incognitas de um monómio
findMax:: [([Char] ,Int)]-> Int
findMax [] = 0
findMax [a] = snd a
findMax (x:xs) = if( (snd x)>= (snd (head xs))) then findMax ([x]++(tail xs))
            else findMax xs

-- Ordena a lista de incognitas por ordem crescente do seu expoente
sortMon :: Ord a => Ord b => [(a, b)] -> [(a, b)]
sortMon = sortBy (compare `on` snd)

-- Ordena os monómios por ordem decrescente do seu maior expoente
sortPol z = reverse(sortOn (\(x,y)->(findMax  x)) z) 


----  FUNÇÕES PARA IMPRIMIR COMO STRING

-- Passa as variáveis de monómio para string (x^2)
stringifyVar :: [(String, Int)] -> String
stringifyVar a = foldl (++) "" (map (\x -> if ((snd x) /= 1) then (fst x ++ "^" ++ (show (snd x))) else (fst x)) (sortMon a))

-- Transforma um monómio em string
stringifyMon :: ([(String, Int)], [Int]) -> String 
stringifyMon ([], a) = show (a !! 0)
stringifyMon a = show (abs ((snd a) !! 0)) ++ "*" ++ stringifyVar (fst a)

-- Transforma um polinómio em string (adiciona os sinais entre os monómios )
stringifyPol :: [([(String, Int)], [Int])] -> String
stringifyPol a = foldl (++) "" (map (\x -> if (((snd x) !! 0) > 0 ) then (" + " ++ (stringifyMon x)) else (" - " ++ (stringifyMon x))) a)

-- Verifica o valor do primeiro monómio e adiciona um "-" ou nada no início do polinómio
stringify :: [([(String, Int)], [Int])] -> String
stringify (x:xs) = if ((snd x) !! 0 > 0) then (stringifyMon x) ++ (stringifyPol xs) else "- " ++ (stringifyMon x) ++ (stringifyPol xs)


-- A) NORMALIZAÇÃO

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

-- soma coeficientes de dois monómios (com variáveis e expoentes iguais)
joinMon :: ([(String, Int)], [Int]) -> ([(String, Int)], [Int]) -> ([(String, Int)], [Int])
joinMon x y = ( fst y, [sum ((snd y) ++ (snd x))] )

-- soma os coeficientes de vários monómios (com variáveis e expoentes iguais)
joinMonList :: [([(String, Int)], [Int])]  -> [([(String, Int)], [Int])]
joinMonList (x:xs) = [foldl (\z y -> joinMon z y) x xs]

-- soma os monómios com variáveis e expoentes iguais dentro de um polinómio
joinPoly :: [([(String, Int)], [Int])]  -> [([(String, Int)], [Int])]
joinPoly [] = []
joinPoly (x:xs) = (joinMonList ([(fst x, [sum (snd x)])] ++ (filter (\y -> isEqual (fst x) (fst y)) xs))) ++ joinPoly (filter (\y -> not (isEqual (fst x) (fst y))) xs)

-- remove os monómios com coeficiente zero ( == 0 ) ou sem coeficiente ( == [] )
removeMon :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
removeMon a = filter (\x -> (not (((snd x) !! 0) ==0) && ( not ((snd x)==[])))) a

-- apaga incógnitas com expoente 0
removeNull :: [(String, Int)] -> [(String, Int)]
removeNull [] = []
removeNull (x:xs) = if snd x == 0 then removeNull xs else x : removeNull xs

removeNullExp :: ([(String, Int)], [Int]) -> ([(String, Int)], [Int])
removeNullExp a = addEqualVar(removeNull(fst a), snd a)

removeVar :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
removeVar xs = map removeNullExp xs


----   FUNÇÕES DE ORDENAÇÃO

--Função auxiliar : encontra o maior expoente de entre todas as incognitas de um monómio
findMax:: [([Char] ,Int)]-> Int
findMax [] = 0
findMax [a] = snd a
findMax (x:xs) = if( (snd x)>= (snd (head xs))) then findMax ([x]++(tail xs))
            else findMax xs

-- Ordena a lista de incognitas por ordem crescente do seu expoente
sortMon :: Ord a => Ord b => [(a, b)] -> [(a, b)]
sortMon = sortBy (compare `on` snd)

-- Ordena os monómios por ordem decrescente do seu maior expoente
sortPol z = reverse(sortOn (\(x,y)->(findMax  x)) z) 


----  FUNÇÕES PARA IMPRIMIR COMO STRING

-- Passa as variáveis de monómio para string (x^2)
stringifyVar :: [(String, Int)] -> String
stringifyVar a = foldl (++) "" (map (\x -> if ((snd x) /= 1) then (fst x ++ "^" ++ (show (snd x))) else (fst x)) (sortMon a))

-- Transforma um monómio em string
stringifyMon :: ([(String, Int)], [Int]) -> String 
stringifyMon ([], a) = show (a !! 0)
stringifyMon a = show (abs ((snd a) !! 0)) ++ "*" ++ stringifyVar (fst a)

-- Transforma um polinómio em string (adiciona os sinais entre os monómios )
stringifyPol :: [([(String, Int)], [Int])] -> String
stringifyPol a = foldl (++) "" (map (\x -> if (((snd x) !! 0) > 0 ) then (" + " ++ (stringifyMon x)) else (" - " ++ (stringifyMon x))) a)

-- Verifica o valor do primeiro monómio e adiciona um "-" ou nada no início do polinómio
stringify :: [([(String, Int)], [Int])] -> String
stringify [] = ""
stringify (x:xs) = if ((snd x) !! 0 > 0) then (stringifyMon x) ++ (stringifyPol xs) else "- " ++ (stringifyMon x) ++ (stringifyPol xs)

-- 1. NORMALIZAÇÃO
-- Normaliza um polinómio
normal :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
normal a =  map (\x -> (sortMon (fst x), snd x)) (sortPol (removeVar (removeMon (joinPoly a))))

-- Imprime o polinómio normalizado na forma de String
stringifyNormal ::  [([(String, Int)], [Int])]  -> String
stringifyNormal y = stringify  (normal y)


-- B) SOMA

-- Soma dois polinómios e imprime na forma de string
sumPoly :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])] -> String
sumPoly a b = stringify (normal (a ++ b))

sumString :: String -> String -> String
sumString a b = sumPoly (parse a) (parse b)

-- Soma vários polinómios numa lista e imprime na forma de string
sumPolyList :: [[([(String, Int)], [Int])]] -> String
sumPolyList a = stringify (normal (foldl (++) [] a))

sumListString :: [String] -> String
sumListString a = sumPolyList (map parse a)


-- C) MULTIPLICAÇÃO

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
mul :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])] -> String
mul a b =  stringify (normal (mulPoly a b))

mulString :: String -> String -> String
mulString a b = mul (parse a) (parse b)


-- D) DERIVAÇÃO

-- mon não tem String -> 0
-- mon tem String e mais nada -> exp -1; coef * exp
-- mon tem String e mais -> derivar String * resto 

stringIn :: [(String, Int)] -> String -> Bool
stringIn [] b = False
stringIn (x:xs) b = if fst x == b then True else stringIn xs b

-- derivação com string
deriveVar :: [(String, Int)] -> [Int] -> ([(String, Int)], [Int])
deriveVar a [b] = ([(fst(a!!0), snd(a!!0)-1)], [b*snd(a!!0)])

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
deriveMul a b = concatVar(deriveVar (filterVar (fst a) b) (snd a)) (notFilterVar (fst a) b)

deriveMon :: ([(String, Int)], [Int]) -> String -> ([(String, Int)], [Int])
deriveMon a b = if stringIn (fst a) b then deriveMul a b else ([],[0])

derive :: [([(String, Int)], [Int])] -> String -> String
derive a b = stringify (normal (map (\x -> deriveMon x b) (normal a)))

deriveString :: String -> String -> String
deriveString a b = derive (parse a) b

-- Normal
--[([("x",1),("y",3)],[3,5]),([("x",7)],[9]),([("y",3),("x",1)],[7])]  -> "9*x^7 + 15*xy^3"
--[([("x",1)],[0]),([("z",7)],[9]),([("y",3),("x",1)],[])]  ->  "9*z^7"
--[([("x",1)],[1]),([("x",1)],[-2]),([("x",1)],[])]  ->  "3*x"
--[([("x",1)],[1]),([("x",1)],[-2]),([("x",1)],[])]  ->  "- 1*x"
-- stringifyNormal [] -> ""


-- Soma
-- sumString "- 2*x^2 + 3*y^5 -x"  "- 2*x^2 + 0*y^5 -3*x"
-- sumString "- 2*x^2 + 3*y^5 -x"  "- 2*x^2 + 3*y^5 -x"
-- sumString "- 2*x^2 + 3*y^5 -x"  ""

-- Multiplicação



--myPredicate (a1, a2) (b1, b2) = compare a1 b1 `mappend` compare a2 b2
--sortMon = sortBy (myPredicate) 
