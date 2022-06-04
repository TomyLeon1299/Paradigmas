{-
Parcial Funcional PdeP

Fecha:
Nombre: Tomás Guillermo León
Curso: K2001
Legajo: 1713980
DNI: 42101908
-}

import Text.Show.Functions()

data Serie = Serie{
    nombre :: String,
    actores :: [Actor],
    presupuesto :: Int,
    temporadas :: Int,
    rating :: Int,
    cancelada :: Bool
}deriving(Show, Eq)

data Actor = Actor{
    nombreActor :: String,
    sueldo :: Int,
    restricciones :: [String]
}deriving(Show, Eq)

type Produccion = Serie -> Serie

-- Mapeos

mapNombre :: (String -> String) -> Serie -> Serie
mapNombre unaFuncion serie = serie { nombre = unaFuncion (nombre serie) }

mapActores :: ([Actor] -> [Actor]) -> Serie -> Serie
mapActores unaFuncion serie = serie { actores = unaFuncion (actores serie) }

mapPresupuestos :: (Int -> Int) -> Serie -> Serie
mapPresupuestos unaFuncion serie = serie { presupuesto = unaFuncion (presupuesto serie) }

mapTemporadas :: (Int -> Int) -> Serie -> Serie
mapTemporadas unaFuncion serie = serie { temporadas = unaFuncion (temporadas serie) }

mapCancelada :: (Bool -> Bool) -> Serie -> Serie
mapCancelada funcion serie = serie { cancelada = funcion (cancelada serie) }

-- PUNTO 1

estaEnRojo :: Serie -> Bool
estaEnRojo serie = (presupuesto serie) < (sum . map sueldo $ (actores serie))

esProblematica :: Serie -> Bool
esProblematica serie = (tieneRestricciones 1 serie) > 3

tieneRestricciones :: Int -> Serie -> Int
tieneRestricciones numero serie = length (filter (masDeXRestricciones numero) (actores serie))

masDeXRestricciones :: Int -> Actor -> Bool
masDeXRestricciones numero = (> numero) . length . restricciones

-- PUNTO 2

-- a

conFavoritismo :: [Actor] -> Produccion
conFavoritismo actoresFavoritos = reemplazarActores actoresFavoritos . (eliminarActores 2)

eliminarActores :: Int -> Serie -> Serie
eliminarActores numero = mapActores (drop numero)

reemplazarActores :: [Actor] -> Serie -> Serie
reemplazarActores actoresFavoritos = mapActores (++ actoresFavoritos)

-- b

timBurton :: Produccion
timBurton = conFavoritismo [johnnyDepp, helenaBonhamCarter]

johnnyDepp :: Actor
johnnyDepp = Actor "Johnny Depp" 20000000 []

helenaBonhamCarter :: Actor
helenaBonhamCarter = Actor "Helan Bonham Carter" 15000000 []

-- c

gatopardeitor :: Produccion
gatopardeitor = id

-- d

estireitor :: Produccion
estireitor = mapTemporadas (*2)

-- e

desespereitor :: Produccion
desespereitor = gatopardeitor . estireitor . timBurton

-- f

canceleitor :: Int -> Produccion
canceleitor limiteRating serie
    | estaEnRojo serie || (rating serie) < limiteRating = mapCancelada (const True) serie
    | otherwise = serie

-- PUNTO 3

bienestar :: Serie -> Int
bienestar serie
    | cancelada serie == True = 0
    | otherwise = bienestarReparto serie + bienestarLongitud serie

bienestarReparto :: Serie -> Int
bienestarReparto serie
    | temporadas serie > 4 = 5
    | otherwise = (10 - temporadas serie) * 2

bienestarLongitud :: Serie -> Int
bienestarLongitud serie
    | length (actores serie) < 10 = 3
    | otherwise = 10 - (tieneRestricciones 2 serie)

-- PUNTO 4

productorMasEfectivo :: [Serie] -> [Produccion] -> [Serie]
productorMasEfectivo series productores = map (masEfectivo productores) series

masEfectivo :: [Produccion] -> Serie -> Serie
masEfectivo [] serie = serie
masEfectivo (x:[]) serie = x serie
masEfectivo (x1:x2:xs) serie
    | bienestar (x1 serie) > bienestar (x2 serie) = masEfectivo (x1:xs) serie
    | otherwise = masEfectivo (x2:xs) serie

-- PUNTO 5

{-
a. ¿Se puede aplicar el productor gatopardeitor cuando tenemos una lista infinita de actores?
se puede aplicar ya que la funcion gatopardeitor es basicamente la funcion id, esto hace que se muestre por consola la lista infinita, pero siempre va a mostrar la lista infinita de actores
y nunca va a pasar a las siguientes parametros del data Serie, entonces funciona pero no cumple con lo deseado.

b. ¿Y a uno con favoritismos? ¿De qué depende?
en este caso no se puede, ya que los primeros dos actores si se pueden eliminar, pero despues en el caso de mi funcion, concatena los actores favoritos al final,
y esto es imposible ya que la lista es infinita y nunca encuentra el final. Si la funcion concatenara los actores al principio de la lista de actores si se podria hacer.
-}

actoresInfinitos :: [Actor]
actoresInfinitos = johnnyDepp : actoresInfinitos

serieEjemplo :: Serie
serieEjemplo = Serie "serie ejemplo" actoresInfinitos 100 2 5 False

-- PUNTO 6

esControvertida :: Serie -> Bool
esControvertida serie = not $ cobraMasQueElSiguiente (actores serie)

cobraMasQueElSiguiente :: [Actor] -> Bool
cobraMasQueElSiguiente [] = True
cobraMasQueElSiguiente (x:[]) = True
cobraMasQueElSiguiente (x:xs)
    | (sueldo x) > (sueldo (head xs)) = cobraMasQueElSiguiente xs
    | otherwise = False

-- PUNTO 7

funcionLoca :: (Int -> Int) -> (a -> [b]) -> [a] -> [Int]
funcionLoca x y = filter (even.x) . map (length.y)

-- primero sabemos que hay dos parametro : x e y
-- como la primer funcion que se va a aplicar es map, sabemos que hay un tercer parametro implicito: z
-- z es una lista, no sabemos de que
-- funcionLoca :: -> -> [a] -> 
-- como y recibe la lista de z, debe tener su mismo tipo, pero puede devolver algo de otro tipo. lo unico que 
-- sabemos de este algo es que debe ser una lista, pues luego se le aplica la funcion length
-- funcionLoca :: -> (a -> [b]) -> [a] -> 
-- luego, se aplica filter. sabemos que el map devuelve una lista de Int y que sobre esa lista se aplicara el filter.
-- por lo que x es una funcion que recibe Int y devuelve un Int (ya que luego se le aplica even)
-- finalmente la funcion funcionLoca devuelve una lista de Int:
-- funcionLoca :: (Int -> Int) -> (a -> [b]) -> [a] -> [Int]