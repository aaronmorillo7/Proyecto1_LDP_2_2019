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