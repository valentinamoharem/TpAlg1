module Auxiliares
(pertenece,
longitud,
esPrimo,
sumaDivisores,
sumaDivisoresHasta)
where

-- definiendo 'círculo' utilizando type
type Circulo = [Integer]

-- pertenece chequea si un entero pertenece o no a un Círculo
pertenece :: Integer -> Circulo -> Bool
pertenece n circulo | circulo == [] = False
                    | (head circulo) == n = True
                    | otherwise = pertenece n (tail circulo)

-- longitud retorna un entero que representa la cantidad de elementos de un conjunto
longitud :: Circulo -> Int
longitud [] = 0
longitud circulo = length circulo

-- sumaDivisores encuentra todos los divisores del entero n y los suma
sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresHasta n n  

-- sumaDivisoresHasta es la función auxiliar de sumaDivisores y efectúa la suma limitando los divisores
-- hasta n
sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n k | k == 1 = 1
                       | n `mod` k == 0 = (sumaDivisoresHasta n (k-1)) + k
                       | otherwise = sumaDivisoresHasta n (k-1)

-- esPrimo chequea si un entero n es o no primo, retorna True o False respectivamente
esPrimo :: Integer -> Bool
esPrimo n = (sumaDivisores n) == (n+1)
