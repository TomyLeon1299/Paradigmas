{-
Parcial Funcional PdeP "Haskell Chef"

Fecha:06/06/2022
Nombre: Tomás Guillermo León
Curso: K2001
Legajo: 1713980
DNI: 42101908
-}

import Text.Show.Functions()

-- PARTE A

data Parcitipante = Parcitipante{
    nombre :: String,
    trucos :: [Truco],
    especialidad :: Plato
}deriving(Show)

data Plato = Plato{
    dificultad :: Int,
    ingredientes :: [Ingrediente]     
}deriving(Show, Eq)

type Ingrediente = (String, Int)         -- (nombrePlato, gramos)
type Truco = Plato -> Plato

-- Funciones Auxiliares

mapIngredientes :: ([Ingrediente]  -> [Ingrediente]) -> Plato -> Plato
mapIngredientes unaFuncion plato = plato { ingredientes = unaFuncion (ingredientes plato) }

mapDificultad :: (Int -> Int) -> Plato -> Plato
mapDificultad unaFuncion plato = plato { dificultad = unaFuncion (dificultad plato) }

agregarIngrediente :: Ingrediente -> Plato -> Plato
agregarIngrediente ingredienteNuevo = mapIngredientes (++ [ingredienteNuevo])

nombreIngredientesPlato :: Plato -> [String]
nombreIngredientesPlato = map fst . ingredientes

gramosIngredientesPlato :: Plato -> [Int]
gramosIngredientesPlato = map snd . ingredientes

tieneIngrediente :: String -> Plato -> Bool
tieneIngrediente ingrediente plato = elem ingrediente (nombreIngredientesPlato plato)

-- Trucos

endulzar :: Int -> Truco
endulzar gramosAzucar = agregarIngrediente ("azucar", gramosAzucar)

salar :: Int -> Truco
salar gramosSal = agregarIngrediente ("sal", gramosSal)

darSabor :: Int -> Int -> Truco
darSabor gramosAzucar gramosSal = endulzar gramosAzucar . salar gramosSal

duplicarPorcion :: Truco
duplicarPorcion plato = plato {ingredientes = map duplicarIngredientes . ingredientes $ plato}

duplicarIngredientes :: Ingrediente -> Ingrediente
duplicarIngredientes (nombreIngrediente, gramos) = (nombreIngrediente, gramos * 2)

simplificar :: Truco
simplificar plato
    | esComplejo plato = mapDificultad (const 5) . mapIngredientes(const (ingredientesMayorA10 (ingredientes plato))) $ plato
    | otherwise = plato

ingredientesMayorA10 :: [Ingrediente] -> [Ingrediente]
ingredientesMayorA10 = filter (mayorA10Gramos)

mayorA10Gramos :: Ingrediente -> Bool
mayorA10Gramos ingrediente = snd ingrediente > 10 

-- De los platos nos interesa saber

esVegano :: Plato -> Bool
esVegano = all (\x -> notElem x ["carne", "huevo", "queso", "leche", "crema"]) . nombreIngredientesPlato

esSinTacc :: Plato -> Bool
esSinTacc = not . tieneIngrediente "harina"

esComplejo :: Plato -> Bool
esComplejo plato = length (ingredientes plato) > 5 && dificultad plato > 7

noAptoHipertension :: Plato -> Bool
noAptoHipertension plato = any tieneMuchaSal (ingredientes plato)

tieneMuchaSal :: Ingrediente -> Bool
tieneMuchaSal (nombreIngrediente, gramos) = nombreIngrediente == "sal" && gramos > 2

-- PARTE B

pepeRonccino :: Parcitipante
pepeRonccino = Parcitipante "Pepe Ronccino" [darSabor 5 2, simplificar, duplicarPorcion] platoPepe

platoPepe :: Plato
platoPepe = Plato 12 [("sal", 20), ("pollo", 500), ("crema", 100), ("pimienta", 10), ("vino", 50), ("caldo", 70)]

-- PARTE C

cocinar :: Parcitipante -> Plato
cocinar participante = foldr ($) (especialidad participante) (trucos participante)

esMejorQue :: Plato -> Plato -> Bool
esMejorQue plato1 plato2 = esMasDificil plato1 plato2 && esMasLiviano plato1 plato2

esMasDificil :: Plato -> Plato -> Bool
esMasDificil plato1 plato2 = dificultad plato1 > dificultad plato2

esMasLiviano :: Plato -> Plato -> Bool
esMasLiviano plato1 plato2 = sumaPesos plato1 < sumaPesos plato2

sumaPesos :: Plato -> Int
sumaPesos = sum . gramosIngredientesPlato

participanteEstrella :: [Parcitipante] -> Parcitipante
participanteEstrella [] = undefined
participanteEstrella [participante] = participante
participanteEstrella (participante1 : participante2 : participantes)
    | esMejorQue (cocinar participante1) (cocinar participante2) = participanteEstrella (participante1 : participantes)
    | otherwise = participanteEstrella (participante2 : participantes)

-- PARTE D

platinum :: Plato
platinum = Plato 10 (zip nombreIngredientes [1..])

nombreIngredientes :: [String]
nombreIngredientes = zipWith (++) (repeat "Ingrediente ") (map show [1..])

{-
¿Qué sucede si aplicamos cada uno de los trucos modelados en la Parte A al platinum?
En las funciones endulzar salar y darSabor pasa lo mismo, no se puede, 
ya que como yo use ++ esto hace que el ingrediente nuevo se agrega al final de la lista, 
entonces esto es imposible ya que nunca va a llegar al final de dicha lista por ser infinita, entonces no puede agregar el ingrediente.
Esto si se podia hacer si en vez de usar ++ uso : ya que de esta forma el ingrediente nuevo se agrega al principio de la lista,
esto ocurre gracias a la lazy evaluation, que no lee toda la lista sino que simplemente lo agrega adelante y listo.
En duplicarPorcion se puede pero no terminaria nunca, ya que no podriamos duplicar los gramos de infinitos ingredientes.
Y en simplificar tambien se puede pero no terminaria nunca, ya que nunca terminaria de filtrar los ingredientes que tengan peso menor a 10.

¿Cuáles de las preguntas de la Parte A (esVegano, esSinTacc, etc.) se pueden responder sobre el platinum? 
En es esVegano y esSinTacc se podria pero no terminaria nunca porque usamos los ingredientes, ya sea ver si en la lista de ingredientes hay carne huevos queso, o si tiene harina, 
entonces nunca terminaria de buscar si en esa lista hay dichos ingredientes.
En esComplejo no se puede, ya que la dificultad si se puede comparar, pero no se puede saber la longitud de una lista infinita.
Y en noAptoHipertension no se puede ya que no puede filtrar la cantidad de sal que tiene un plato ya que nunca va a terminar de buscar sal en la lista de ingredientes.

¿Se puede saber si el platinum es mejor que otro plato?
Se puede pero no terminaria nunca, ya que para saber cual es el mejor necesitamos ver que la dificultad sea mayor, esto si se puede hacer, gracias a la lazy evaluation que solo va a buscar la dificultad del plato y no nos interesa lo demas,
pero para que el plato sea mejor tambien tenemos que ver la suma de los gramos de sus ingredientes y no terminaria nunca de sumar.
-}