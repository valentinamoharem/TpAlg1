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
compararCirculosPorOrden [] _ = True
compararCirculosPorOrden circulo1 circulo2
                            | (lC1 == 1 && lC2 == 1) = True 
                            | lC1 > 1 && lC2 > 1 = compararCirculosConOtro circulo1 circulo2 []
                            | otherwise = False
                        where 
                            lC1 = toInteger (length circulo1)
                            lC2 = toInteger (length circulo2)
                            
compararCirculosConOtro :: Circulo -> Circulo -> Circulo -> Bool 
compararCirculosPorOrden [] [] cAux = True
compararCirculosPorOrden (cc1:c1) (cc2:c2) cAux
                            | c1 == c2 = True
                            | otherwise = compararElemYSumar cc1 cc2 cAux

compararElemYSumar :: Circulo -> Circulo -> Circulo -> Bool
compararElemYSumar [] [] cAux = True
compararElemYSumar (cc1:c1) (cc2:c2) cAux
                                | cc1 == cc2                              

-- posicionDeElemEnCirculo retorna la posición de un entero n dentro de un círculo
posicionDeElemEnCirculo :: Integer -> Circulo -> Integer
posicionDeElemEnCirculo n (c:circulo)
                    | (pertenece n circulo) == False = 0
                    | (n == c) = 1
                    | otherwise = 1 + posicionDeElemEnCirculo n circulo
