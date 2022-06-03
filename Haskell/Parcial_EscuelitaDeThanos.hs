import Text.Show.Functions()

type Gema = Personaje -> Personaje
type Universo = [Personaje]

data Personaje = Personaje {
    nombre :: String,
    edad :: Int,
    habilidades :: [String],
    planeta :: String,
    energia :: Int
}deriving(Show, Eq)

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
}deriving(Show)

-- PUNTO 1

chasquido :: Guantelete -> Universo -> Universo
chasquido guantelete universo
    | puedeUsarse guantelete = reducirALaMitad universo
    | otherwise = universo

puedeUsarse :: Guantelete -> Bool
puedeUsarse guantelete = length (gemas guantelete) == 6 && material guantelete == "uru"

reducirALaMitad :: Universo -> Universo
reducirALaMitad universo = take (length universo `div` 2) universo

-- PUNTO 2

universoAptoParaPendex :: Universo -> Bool
universoAptoParaPendex universo = any ((<=45).edad) universo

energiaTotalDelUniverso :: Universo -> Int
energiaTotalDelUniverso = sum.map energia.filter ((>1).length.habilidades)

-- PUNTO 3

mente :: Int -> Gema
mente = sacarEnergia

sacarEnergia :: Int -> Gema
sacarEnergia valor personaje = personaje {energia = energia personaje - valor}

alma :: String -> Gema
alma habilidad personaje = sacarEnergia 10 personaje {habilidades = filter (/= habilidad) (habilidades personaje)}

espacio :: String -> Gema
espacio nuevoPlaneta personaje = sacarEnergia 20 personaje {planeta = nuevoPlaneta}

poder :: Gema
poder personaje = sacarHabilidad.(sacarEnergia (energia personaje)) $ personaje

sacarHabilidad :: Gema
sacarHabilidad personaje
    | (<=2).length.habilidades $ personaje = personaje {habilidades = []}
    | otherwise = personaje

tiempo :: Gema
tiempo personaje = personaje{edad = max 18 ((edad personaje) `div` 2)}

gemaLoca :: Gema -> Gema
gemaLoca gema = gema.gema

-- PUNTO 4

guanteleteDeGoma :: Guantelete
guanteleteDeGoma = Guantelete {material = "Goma", gemas = [tiempo, alma "usar Mjolnir", gemaLoca (alma "programacion en Haskell")]}

-- PUNTO 5

utilizar :: [Gema] -> Gema
utilizar listaGemas personaje = foldr ($) personaje $ listaGemas

-- PUNTO 6

gemaMasPoderosa :: Personaje -> Guantelete -> Gema
gemaMasPoderosa personaje guantelete = (gemaMasPoderosaDe personaje).gemas $ guantelete

gemaMasPoderosaDe :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDe _ [] = id
gemaMasPoderosaDe _ [gema] = gema
gemaMasPoderosaDe personaje (gema1:gema2:gemas)
    | (energia.gema1) personaje < (energia.gema2) personaje = gemaMasPoderosaDe personaje (gema1:gemas)
    | otherwise = gemaMasPoderosaDe personaje (gema2:gemas)

-- PUNTO 7

infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

{-
gemaMasPoderosa punisher guanteleteDeLocos
No se puede ejercutar, ya que la lista de gemas es infinita y en este caso cuando quiere ver cual es la gema mas poderosa nunca va a terminar de leer esa lista.

usoLasTresPrimerasGemas guanteleteDeLocos punisher
Si se puede ejecutar, ya que en este caso la lista es infinita pero como solo quiere las primeras 3 de dicha lista, gracias a la lazy evaluation no lee toda la lista antes de aplicar la funcion. 
-}