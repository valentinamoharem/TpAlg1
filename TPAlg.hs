import Auxiliares
-- recordatorio: pasar auxiliares a este documento
-- TP Álgebra I, Taller, 2do Cuatrimestre 2019

-- definiendo 'círculo' utilizando type
type Circulo = [Integer]

-- EJERCICIO 1
-- sonCirculosIguales :: Circulo -> Circulo -> Bool
-- sonCirculosIguales circulo1 circulo2
-- que determine si circulo1 y circulo2 son iguales (teniendo en cuenta que dos círculos son iguales
-- si difieren por rotación, y además que cada círculo puede representarse mediante más de una lista).

-- PROBLEMA: empty list

sonCirculosIguales :: Circulo -> Circulo -> Bool
sonCirculosIguales [] [] = True
sonCirculosIguales circulo1 [] = False
sonCirculosIguales circulo1 circulo2 
                    | lC1 /= lC2 = False
                    | lC1 == 0 && lC2 == 0 = True
                    | lC1 == 1 && lC2 == 1 = True
                    | lC1 == 2 && lC2 == 2 = True
                    | (compararCirculosPorOrden circulo1 circulo2) == True = True
                    | otherwise = False
                where
                    lC1 = toInteger (length circulo1)
                    lC2 = toInteger (length circulo2)

-- compararCirculosPorOrden 
compararCirculosPorOrden :: Circulo -> Circulo -> Bool
compararCirculosPorOrden [] [] = True
compararCirculosPorOrden [x] [y] = True
compararCirculosPorOrden (x:circulo1) (y:circulo2)
                            | ((pH1 == pHT1 - 1) || (pH1 == lC2) && (pHT1 == 1)) = compararCirculosPorOrden (tail circulo1) circulo2
                            | otherwise = False
                        where 
                            pH1 = posicionDeElemEnCirculo (head circulo1) circulo2
                            pHT1 = posicionDeElemEnCirculo (head (tail circulo1)) circulo2
                            lC1 = toInteger (length circulo1)
                            lC2 = toInteger (length circulo2)

-- posicionDeElemEnCirculo retorna la posición de un entero n dentro de un círculo
posicionDeElemEnCirculo :: Integer -> Circulo -> Integer
posicionDeElemEnCirculo n (c:circulo)
                    | (pertenece n circulo) == False = 0
                    | (n == c) = 1
                    | otherwise = 1 + posicionDeElemEnCirculo n circulo

----------------------------------------------------------------------------------------------------

-- EJERCICIO 3
-- esCirculoPrimo :: Circulo -> Bool
-- esCirculoPrimo circulo
-- que determine si circulo cumple las condiciones necesarias para ser un círculo primo. Se puede
-- asumir que el círculo es de orden mayor a 1.

esCirculoPrimo :: Circulo -> Bool
esCirculoPrimo [] = False
esCirculoPrimo circulo 
                | sumaPrimoCirculo circulo == True = True
                | otherwise = False

-- sumaPrimo chequea si la suma de dos números da como resultado un número primo o no, retorna True o False respectivamente
sumaPrimoCirculo :: Circulo -> Bool
sumaPrimoCirculo [] = True
sumaPrimoCirculo (c:circulo) 
                    | esPrimo ((head circulo) + (head (tail circulo))) == True = sumaPrimoCirculo (tail circulo)
                    | otherwise = False

-- NOTAS
-- 1- Problema: empty list
-- 2- No chequea de forma cíclica, falta agregar que chequee sumaPrimoCirculo entre el último elemento del conjunto
-- y su head  
-----------------------------------------------------------------------------------------------------