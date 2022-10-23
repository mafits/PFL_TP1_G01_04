import Data.List
import Data.Ord
import Data.Char
import Data.Function
import Data.Monoid (mappend)

----   PARSING: de string para polinómio

-- Remove os espaços nas strings de input
removeSpace :: String -> String
removeSpace = filter (not . isSpace)

-- Testa se a componente do monómio tem uma variável (uma letra)
hasLetter :: String -> Bool
hasLetter [] = False
hasLetter (x:xs) = if isLetter x then True else hasLetter xs

-- Transforma uma incógnita na representação interna
parseVar :: String -> (String, Int)
parseVar a | length a == 1 = (a, 1)
           | otherwise = (takeWhile (/= '^') a, read (drop 2 a) :: Int)

-- Transforma o coeficiente na representação interna
parseCoef :: String -> Int
parseCoef a = read a :: Int

-- Dividi um monómio numa lista de componentes (coeficiente e incógnitas)
divideInComp :: String -> [String]
divideInComp [] = []
divideInComp ('*':xs) = divideInComp(xs)
divideInComp a | (head (takeWhile (/= '*') a) == '-') && isLetter (takeWhile (/= '*') a !! 1) = "-1" : tail (takeWhile (/= '*') a) : divideInComp (dropWhile (/= '*') a)
               | otherwise = takeWhile (/= '*') a : divideInComp (dropWhile (/= '*') a)

-- Tranforma um monómio na representação interna
parseMon :: String -> ([(String, Int)], [Int])
parseMon (x:xs) | length (filter (not . hasLetter) (divideInComp (x:xs))) == 0 = (map parseVar (filter hasLetter (divideInComp (x:xs))), [1])
                | otherwise = (map parseVar (filter hasLetter (divideInComp (x:xs))), map parseCoef (filter (not . hasLetter) (divideInComp (x:xs))))

-- Transforma um polinómio em String (já sem espaços)
parsePoly :: String -> [([(String, Int)], [Int])]
parsePoly [] = []
parsePoly ('+':xs) = parsePoly xs
parsePoly (x:xs) | x == '-' = parseMon (x : takeWhile (\x -> (x /= '+') && (x /= '-')) xs) : parsePoly (dropWhile (\x -> (x /= '+') && (x /= '-')) xs)
                 | otherwise = parseMon (takeWhile (\x -> (x /= '+') && (x /= '-')) (x:xs)) : parsePoly (dropWhile (\x -> (x /= '+') && (x /= '-')) (x:xs))

-- Parsing do monómio, após retirar os espaços
parse :: String -> [([(String, Int)], [Int])]
parse a = parsePoly(removeSpace a)


----   FUNÇÕES DE ORDENAÇÃO

--Função auxiliar : encontra o maior expoente de entre todas as incognitas de um monómio
findMax:: [([Char] ,Int)]-> Int
findMax [] = 0
findMax [a] = snd a
findMax (x:xs) = if( (snd x)>= (snd (head xs))) then findMax ([x]++(tail xs))
            else findMax xs

-- Ordena a lista de incognitas por ordem crescente do seu expoente; Quando estes são iguais ordena por ordem alfabética
sortMon :: Ord a => Ord b => [(a, b)] -> [(a, b)]
sortMon = sortBy ((compare `on` snd) `mappend` (compare `on` fst))

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


-- A) NORMALIZAÇÃO

-- Testa se uma incógnita está presente numa lista de incógnitas
exists:: (String, Int) -> [(String, Int)] -> Bool
exists a [] = False
exists a (b:xs) | a == b = True
                | otherwise = exists a xs

-- Testa se as incógnitas de dois polinómios são iguais
isEqual:: [(String, Int)] -> [(String, Int)] -> Bool
isEqual [] b = True
isEqual (a:xs) b | exists a b = isEqual xs b
                 | otherwise = False

-- Soma coeficientes de dois monómios (com incógnitas iguais)
joinMon :: ([(String, Int)], [Int]) -> ([(String, Int)], [Int]) -> ([(String, Int)], [Int])
joinMon x y = ( fst y, [sum ((snd y) ++ (snd x))] )

-- Soma os coeficientes de todos os monómios (com incógnitas iguais)
joinMonList :: [([(String, Int)], [Int])]  -> [([(String, Int)], [Int])]
joinMonList (x:xs) = [foldl (\z y -> joinMon z y) x xs]

-- Soma os monómios com incógnitas iguais dentro de um polinómio
joinPoly :: [([(String, Int)], [Int])]  -> [([(String, Int)], [Int])]
joinPoly [] = []
joinPoly (x:xs) = (joinMonList ([(fst x, [sum (snd x)])] ++ (filter (\y -> isEqual (fst x) (fst y)) xs))) ++ joinPoly (filter (\y -> not (isEqual (fst x) (fst y))) xs)

-- Remove os monómios com coeficiente zero ( == 0 ) ou sem coeficiente ( == [] )
removeMon :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
removeMon a = filter (\x -> (not (((snd x) !! 0) ==0) && ( not ((snd x)==[])))) a

-- Apaga a 2a incógnita depois de somada
mydel :: (String, Int) -> [(String, Int)] -> [(String, Int)]
mydel a [] = []
mydel a (x:xs) = if fst(a) == fst(x) then mydel a xs else x : mydel a xs

-- Adiciona expoentes caso a incógnita seja igual
addExp :: (String, Int) -> [(String, Int)] -> (String, Int)
addExp a [] = a
addExp a (x:xs) = if (fst(a) == fst(x)) then addExp (fst(a), snd(a)+snd(x)) xs else addExp a xs

-- Vê se uma lista de incógnitas tem algumas com variável igual
equalVar :: [(String, Int)] -> [(String, Int)]
equalVar [] = []
equalVar (x:xs) = addExp x xs : equalVar(mydel (addExp x xs) xs)

addEqualVar :: ([(String, Int)], [Int]) -> ([(String, Int)], [Int])
addEqualVar a = (equalVar (fst a), snd a)

-- Apaga incógnitas com expoente 0
removeNullExp :: [(String, Int)] -> [(String, Int)]
removeNullExp [] = []
removeNullExp (x:xs) = if snd x == 0 then removeNullExp xs else x : removeNullExp xs

-- Retira expoentes nulos e, num monómio, soma expoentes de incógnitas com a mesma variável
handleVar :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
handleVar a = map (\x -> addEqualVar(removeNullExp(fst x), snd x)) a

-- Normaliza um polinómio (representação interna)
normalPoly :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
normalPoly a =  map (\x -> (sortMon (fst x), snd x)) (sortPol (handleVar (removeMon (joinPoly a))))

-- Normaliza um polinómio (input: representação interna, output: String)
normal ::  [([(String, Int)], [Int])]  -> String
normal y = if (stringify  (normalPoly y)) == "" then "0" else stringify  (normalPoly y)

-- Normaliza um polinómio (String)
normalString ::  String -> String
normalString y = normal (parse y)

-- B) SOMA

-- Soma dois polinómios (representação interna)
sumPoly :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
sumPoly a b = normalPoly (a ++ b)

-- Soma dois polinómios e imprime na forma de string
mysum :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])] -> String
mysum a b = stringify (normalPoly (a ++ b))

-- Soma dois polinómios (String)
sumString :: String -> String -> String
sumString a b = mysum (parse a) (parse b)

-- Soma vários polinómios (input: representação interna, output: String)
sumPolyList :: [[([(String, Int)], [Int])]] -> String
sumPolyList a = stringify (normalPoly (foldl (++) [] a))

-- Soma vários polinómios (String)
sumListString :: [String] -> String
sumListString a = sumPolyList (map parse a)


-- C) MULTIPLICAÇÃO

-- Multiplica 2 monómios
mulMon :: ([(String, Int)], [Int]) -> ([(String, Int)], [Int]) -> ([(String, Int)], [Int])
mulMon x y = (fst x ++ fst y, [(snd x)!!0 * (snd y)!!0])

-- Distribui um monómio por um polinómio
mulMonPoly :: ([(String, Int)], [Int]) -> [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
mulMonPoly x [] = []
mulMonPoly x (y:ys) = (mulMon x y) : mulMonPoly x ys

-- Multiplica dois polinómios (representação interna)
mulPoly :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])] -> [([(String, Int)], [Int])]
mulPoly [] y = []
mulPoly (x:xs) y = mulMonPoly x y ++ mulPoly xs y

-- Multiplica dois polinómios (input: representação interna, output: String)
mul :: [([(String, Int)], [Int])] -> [([(String, Int)], [Int])] -> String
mul a b =  if (stringify (normalPoly (mulPoly a b))) == "" then "0" else (stringify (normalPoly (mulPoly a b)))

-- Multiplica dois polinómios (String)
mulString :: String -> String -> String
mulString a b = mul (parse a) (parse b)


-- D) DERIVAÇÃO

-- Testa se uma variável existe num monómio
stringIn :: [(String, Int)] -> String -> Bool
stringIn [] b = False
stringIn (x:xs) b = if fst x == b then True else stringIn xs b

-- Deriva uma incógnita com a variável igual à que pretendemos derivar
deriveVar :: [(String, Int)] -> [Int] -> ([(String, Int)], [Int])
deriveVar a [b] = ([(fst(a!!0), snd(a!!0)-1)], [b*snd(a!!0)])

-- Filtra as incógnitas com variável igual à que pretendemos derivar
filterVar :: [(String, Int)] -> String -> [(String, Int)]
filterVar [] b = []
filterVar (x:xs) b = if fst x == b then [x] else filterVar xs b

-- Retira das incógnitas as que têm variável igual à que pretendemos derivar
notFilterVar :: [(String, Int)] -> String -> [(String, Int)]
notFilterVar [] b = []
notFilterVar (x:xs) b = if fst x /= b then x : notFilterVar xs b else notFilterVar xs b

-- Adiciona incógnitas a um monómio
concatVar :: ([(String, Int)], [Int]) -> [(String, Int)] -> ([(String, Int)], [Int])
concatVar a b = (fst a ++ b, snd a)

-- Deriva as incógnitas com variável igual à que pretendemos derivar e concatena com as incógnitas que não têm
deriveMul :: ([(String, Int)], [Int]) -> String -> ([(String, Int)], [Int])
deriveMul a b = concatVar(deriveVar (filterVar (fst a) b) (snd a)) (notFilterVar (fst a) b)

-- Deriva um monómio, testando se na listagem de incógnitas tem a variável que pretendemos derivar
deriveMon :: ([(String, Int)], [Int]) -> String -> ([(String, Int)], [Int])
deriveMon a b = if stringIn (fst a) b then deriveMul a b else ([],[0])

-- Deriva um polinómio (representação interna)
derivePoly :: [([(String, Int)], [Int])] -> String -> [([(String, Int)], [Int])]
derivePoly a b = normalPoly (map (\x -> deriveMon x b) (normalPoly a))

-- Deriva um polinómio (input: representação interna, output: String)
derive :: [([(String, Int)], [Int])] -> String -> String
derive a b = if (stringify (derivePoly a b)) == "" then "0" else stringify (derivePoly a b)

-- Deriva um polinómio (String)
deriveString :: String -> String -> String
deriveString a b = derive (parse a) b
