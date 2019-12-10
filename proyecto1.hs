--Obtener la n posicion mas adelante en el alfabeto para Cesar para encriptar

ePosCesar x n = (x + n)`mod`26

--Obtener la n posicion mas adelante en el alfabeto para Cesar para desencriptar

dPosCesar x n = (x - n)`mod`26

posAtbash x = (-x)`mod`26

-- Obtener el numero del caracter que reemplaza al original
-- Llamar a la function de esta manera buscar 'a' (zip [0..25] ['a'..'z'])
buscar::(Num t)=> Char -> [(t, Char)] -> t
buscar c xs = fst((filter (\(_,l)->l==c) xs)!!0)


-- Obtener el caracter que reemplaza al de la i-esima posicion en la cadena original
cesar:: Char -> Int -> (Int->Int->Int) -> Char
cesar c n f 
    | c`elem`mayus = mayus!!(f x n)
    | c`elem`minus = minus!!(f z n)
    | otherwise = c
    where mayus = ['A'..'Z']
          minus = ['a'..'z']
          nums = [0..25]
          x = buscar c (zip nums mayus)
          z = buscar c (zip nums minus)
-- Codificion de Cesar
cesarCod::Int->String->String
cesarCod n xs = map (\x -> cesar x n ePosCesar) xs

cesarDes ::Int->String->String
cesarDes n xs = map (\x -> cesar x n dPosCesar) xs

eatbash:: Char -> Char
eatbash c 
    | c`elem`mayus = mayus!!(posAtbash x)
    | c`elem`minus = minus!!(posAtbash z)
    | otherwise = c
    where mayus = ['A'..'Z']
          minus = ['a'..'z']
          nums = [1..26]
          x = buscar c (zip nums mayus)
          z = buscar c (zip nums minus)
atbash :: String -> String
atbash xs = map (\x -> eatbash x ) xs

-- Cifrado Veginere

--Conversion de letra a indice
conversion :: Char -> Int 
conversion 'a' = 0
conversion 'b' = 1
conversion 'c' = 2
conversion 'd' = 3
conversion 'e' = 4
conversion 'f' = 5
conversion 'g' = 6
conversion 'h' = 7
conversion 'i' = 8
conversion 'j' = 9
conversion 'k' = 10
conversion 'l' = 11
conversion 'm' = 12
conversion 'n' = 13
conversion 'o' = 14
conversion 'p' = 15
conversion 'q' = 16
conversion 'r' = 17
conversion 's' = 18
conversion 't' = 19
conversion 'u' = 20
conversion 'v' = 21
conversion 'w' = 22
conversion 'x' = 23
conversion 'y' = 24
conversion 'z' = 25
conversion 'A' = 0
conversion 'B' = 1
conversion 'C' = 2
conversion 'D' = 3
conversion 'E' = 4
conversion 'F' = 5
conversion 'G' = 6
conversion 'H' = 7
conversion 'I' = 8
conversion 'J' = 9
conversion 'K' = 10
conversion 'L' = 11
conversion 'M' = 12
conversion 'N' = 13
conversion 'O' = 14
conversion 'P' = 15
conversion 'Q' = 16
conversion 'R' = 17
conversion 'S' = 18
conversion 'T' = 19
conversion 'U' = 20
conversion 'V' = 21
conversion 'W' = 22
conversion 'X' = 23
conversion 'Y' = 24
conversion 'Z' = 25 

--Conversion de indice a letra
conversiono :: Int -> Char
conversiono 0 = 'a'
conversiono 1 = 'b'
conversiono 2 = 'c'
conversiono 3 = 'd'
conversiono 4 = 'e'
conversiono 5 = 'f'
conversiono 6 = 'g'
conversiono 7 = 'h'
conversiono 8 = 'i'
conversiono 9 = 'j'
conversiono 10 = 'k'
conversiono 11 = 'l'
conversiono 12 = 'm'
conversiono 13 = 'n'
conversiono 14 = 'o'
conversiono 15 = 'p'
conversiono 16 = 'q'
conversiono 17 = 'r'
conversiono 18 = 's'
conversiono 19 = 't'
conversiono 20 = 'u'
conversiono 21 = 'v'
conversiono 22 = 'w'
conversiono 23 = 'x'
conversiono 24 = 'y'
conversiono 25 = 'z'

--Balance del indice para el recorrido de la palabra clave
balance :: Int->Int->Int
balance a b = if a>=b then 0 else a

--Convertir la frase en base a la palabra 
cambio :: String -> String -> Int -> String
cambio [] _ _ = []
cambio x y b = let c = balance b (length y) in
	if head x /= ' ' && head x /= '=' && head x /= '+' && head x /= ')' && head x /= '(' && head x /= '!' && head x /= '$' && head x /= '^' && head x /= '%' && head x /= '*' && head x /= '#' && head x /= '@' && head x /= '&' && head x /= '/' && head x /= '-' && head x /= '_' then  ([y !! c]++cambio (tail x) y (c+1)) 
	else ([head x]++cambio (tail x) y (c))

cifradot :: String -> String -> Int -> String
cifradot [] _ _ = []
cifradot x y b = let c = balance b (length y); abc = ["abcdefghijklmnopqrstuvwxyz","bcdefghijklmnopqrstuvwxyza","cdefghijklmnopqrstuvwxyzab","defghijklmnopqrstuvwxyzabc","efghijklmnopqrstuvwxyzabcd","fghijklmnopqrstuvwxyzabcde","ghijklmnopqrstuvwxyzabcdef","hijklmnopqrstuvwxyzabcdefg","ijklmnopqrstuvwxyzabcdefgh","jklmnopqrstuvwxyzabcdefghi","klmnopqrstuvwxyzabcdefghij","lmnopqrstuvwxyzabcdefghijk","mnopqrstuvwxyzabcdefghijkl","nopqrstuvwxyzabcdefghijklm","opqrstuvwxyzabcdefghijklmn","pqrstuvwxyzabcdefghijklmno","qrstuvxyzabcdefghijklmnop","rstuvwxyzabcdefghijklmnopq","stuvwxyzabcdefghijklmnopqr","tuvwxyzabcdefghijklmnopqrs","uvwxyzabcdefghijklmnopqrst","vwxyzabcdefghijklmnopqrstu","wxyzabcdefghijklmnoprstuv","xyzabcdefghijklmnopqrstuvw","yzabcdefghijklmnopqrstuvwx", "zabcdefghijklmnopqrstuvwxy"] in
	if head x /= ' ' && head x /= '=' && head x /= '+' && head x /= ')' && head x /= '(' && head x /= '!' && head x /= '$' && head x /= '^' && head x /= '%' && head x /= '*' && head x /= '#' && head x /= '@' && head x /= '&' && head x /= '/' && head x /= '-' && head x /= '_'  then  [(abc!! conversion(head x))!! conversion (y!!c)]++cifradot (tail x) y (c+1) 
	else  ([head x]++cifradot (tail x) y (c+1) ) 

veginereCod :: String -> String -> String 
veginereCod [] []= []
veginereCod [] _ = []
veginereCod x [] = x
veginereCod x y = cifradot x (cambio x y 0) 0 
-- Descifrado Veginere

-- Busqueda el indice 
busqueda :: Int -> Char -> Int -> Int
busqueda a b c = let abc= ["abcdefghijklmnopqrstuvwxyz","bcdefghijklmnopqrstuvwxyza","cdefghijklmnopqrstuvwxyzab","defghijklmnopqrstuvwxyzabc","efghijklmnopqrstuvwxyzabcd","fghijklmnopqrstuvwxyzabcde","ghijklmnopqrstuvwxyzabcdef","hijklmnopqrstuvwxyzabcdefg","ijklmnopqrstuvwxyzabcdefgh","jklmnopqrstuvwxyzabcdefghi","klmnopqrstuvwxyzabcdefghij","lmnopqrstuvwxyzabcdefghijk","mnopqrstuvwxyzabcdefghijkl","nopqrstuvwxyzabcdefghijklm","opqrstuvwxyzabcdefghijklmn","pqrstuvwxyzabcdefghijklmno","qrstuvxyzabcdefghijklmnop","rstuvwxyzabcdefghijklmnopq","stuvwxyzabcdefghijklmnopqr","tuvwxyzabcdefghijklmnopqrs","uvwxyzabcdefghijklmnopqrst","vwxyzabcdefghijklmnopqrstu","wxyzabcdefghijklmnoprstuv","xyzabcdefghijklmnopqrstuvw","yzabcdefghijklmnopqrstuvwx", "zabcdefghijklmnopqrstuvwxy"] in if (abc!!a)!!c==b then c else busqueda a b (c+1)

--Recorrido en ambas frases 
descifradot :: String -> String -> Int -> String
descifradot [] _ _ = []
descifradot x y b = let c=busqueda (conversion (head x)) (head y) 0; abc="abcdefghijklmnopqrstuvwxyz" in 
	if head x /= ' ' && head x /= '=' && head x /= '+' && head x /= ')' && head x /= '(' && head x /= '!' && head x /= '$' && head x /= '^' && head x /= '%' && head x /= '*' && head x /= '#' && head x /= '@' && head x /= '&' && head x /= '/' && head x /= '-' && head x /= '_' then ([abc!!c]++descifradot (tail x) (tail y) (c+1))
	else  ([head x]++descifradot (tail x) (tail y) (c+1) ) 

--Llama principal
veginereDes :: String -> String -> String 
veginereDes [] []= []
veginereDes [] _ = []
veginereDes x [] = x
veginereDes x y = descifradot x y 0
