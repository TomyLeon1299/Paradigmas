{-
Parcial Funcional PdeP

Fecha:
Nombre: Tomás Guillermo León
Curso: K2001
Legajo: 1713980
DNI: 42101908
-}

import Text.Show.Functions()

-- PUNTO 1

data Pizza = Pizza{
    ingredientes :: [String],
    tamanio :: Tamanio,
    calorias :: Int
}

type Tamanio = Int
individual :: Tamanio
individual = 4
chica :: Tamanio
chica = 6
grande :: Tamanio
grande = 8
gigante :: Tamanio
gigante = 10

type Pedido = [Pizza]
type Pizzeria = Pedido -> Pedido

grandeDeMuzza :: Pizza
grandeDeMuzza = Pizza ["salsa", "mozzarella", "oregano"] grande 350

-- Mapeos

mapIngredientes :: ([String] -> [String]) -> Pizza -> Pizza
mapIngredientes unaFuncion pizza = pizza { ingredientes = unaFuncion (ingredientes pizza) }

mapCalorias :: (Int -> Int) -> Pizza -> Pizza
mapCalorias unaFuncion pizza = pizza { calorias = unaFuncion (calorias pizza) }

mapTamanio :: (Tamanio -> Tamanio) -> Pizza -> Pizza
mapTamanio unaFuncion pizza = pizza { tamanio = unaFuncion (tamanio pizza) }


-- PUNTO 2

satisfaccion :: Pizza -> Int
satisfaccion pizza
    | elem "palmito" (ingredientes pizza) = 0
    | calorias pizza < 500 = calcularSatisfaccion pizza
    | otherwise = calcularSatisfaccion pizza `div` 2

calcularSatisfaccion :: Pizza -> Int
calcularSatisfaccion = (*8) . cantidadIngredientes

cantidadIngredientes :: Pizza -> Int
cantidadIngredientes = length . ingredientes

-- PUNTO 3

precioPizza :: Pizza -> Int
precioPizza pizza = (120 * cantidadIngredientes pizza) * (tamanio pizza)

-- PUNTO 4

nuevoIngrediente :: String -> Pizza -> Pizza
nuevoIngrediente ingrediente = mapIngredientes (++ [ingrediente]) . mapCalorias ((+) (length ingrediente))

agrandar :: Pizza -> Pizza
agrandar pizza
    | tamanio pizza == gigante = pizza
    | otherwise = mapTamanio ((+) 2) pizza

mezcladita :: Pizza -> Pizza -> Pizza
mezcladita pizza1 pizza2 = mapIngredientes (++ (ingredientesSinRepetir pizza1 pizza2)) . mapCalorias ((+) (calorias pizza2 `div` 2)) $ pizza1

ingredientesSinRepetir :: Pizza -> Pizza -> [String]
ingredientesSinRepetir pizza1 pizza2 = filter (\x -> notElem x (ingredientes pizza1)) (ingredientes pizza2)

-- PUNTO 5

satisfaccionPedido :: Pedido -> Int
satisfaccionPedido = sum . map satisfaccion

-- PUNTO 6

pizzeriaLosHijosDePato :: Pizzeria
pizzeriaLosHijosDePato = map (mapIngredientes (++ ["palmitos"]))

pizzeriaElResumen :: Pizzeria
pizzeriaElResumen [] = []
pizzeriaElResumen [x] = [x]
pizzeriaElResumen (x1:x2:xs) = mezcladita x1 x2 : pizzeriaElResumen (x2:xs)

pizzeriaEspecial :: Pizza -> Pizzeria
pizzeriaEspecial pizza pedido = map (mezcladita pizza) pedido

anchoas :: Pizza
anchoas = Pizza ["salsa", "anchoas"] grande 270

pizzeriaPescadito :: Pizzeria
pizzeriaPescadito pedido = pizzeriaEspecial anchoas pedido

pizzeriaGourmet :: Int -> Pizzeria
pizzeriaGourmet nivelExquisitez pedido = map agrandar (filter (superaNivelExquisitez nivelExquisitez) pedido)

superaNivelExquisitez :: Int -> Pizza -> Bool
superaNivelExquisitez nivelExquisitez pizza = satisfaccion pizza > nivelExquisitez

pizzeriaLaJauja :: Pizzeria
pizzeriaLaJauja pedido = pizzeriaGourmet 399 pedido

-- PUNTO 7

sonDignasDeCalleCorrientes :: Pedido -> [Pizzeria] -> [Pizzeria]
sonDignasDeCalleCorrientes pedido pizzerias = filter (masSatisfaccion pedido) pizzerias

masSatisfaccion :: Pedido -> Pizzeria -> Bool
masSatisfaccion pedido pizzeria = satisfaccionPedido pedido < satisfaccionPedido (pizzeria pedido)

maximizarSatisfaccion :: Pedido -> [Pizzeria] -> Pizzeria
maximizarSatisfaccion _ [] = undefined
maximizarSatisfaccion _ [x] = x
maximizarSatisfaccion pedido (x1:x2:xs)
    | satisfaccionPedido (x1 pedido) > satisfaccionPedido (x2 pedido) = maximizarSatisfaccion pedido (x1:xs)
    | otherwise = maximizarSatisfaccion pedido (x2:xs)

-- PUNTO 8

yoPidoCualquierPizza :: (Foldable t, Integral b1) => (a -> b1) -> (b2 -> Bool) -> t (a, b2) -> Bool
yoPidoCualquierPizza x y z = any (odd . x . fst) z && all (y . snd) z

-- PUNTO 9

laPizzeriaPredilecta :: [Pizzeria] -> Pizzeria
laPizzeriaPredilecta = undefined