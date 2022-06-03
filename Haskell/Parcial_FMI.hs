import Text.Show.Functions()

-- PUNTO 1

data Pais = Pais{
    ingresoPerCapita :: Float,
    activosPublico :: Int,
    activosPrivado :: Int,
    recursosNaturales :: [String],
    deuda :: Float
}deriving(Show, Eq)

type Estrategia = Pais -> Pais
type Receta = [Estrategia]

namibia :: Pais
namibia = Pais 4140 400000 650000 ["mineria", "ecoturismo"] 50

-- FUNCIONES AUXILIARES

mapDeuda :: (Float -> Float) -> Pais -> Pais
mapDeuda modificador pais = pais {deuda = modificador.deuda $ pais}

mapActivosPublicos :: (Int -> Int) -> Pais -> Pais
mapActivosPublicos modificador pais = pais {activosPublico = modificador.activosPublico $ pais}

mapIngresoPerCapita :: (Float -> Float) -> Pais -> Pais
mapIngresoPerCapita modificador pais = pais {ingresoPerCapita = modificador.ingresoPerCapita $ pais}

-- PUNTO 2

prestarPlata :: Float -> Estrategia
prestarPlata plata = mapDeuda ((+) (cobrarIntereses plata))

cobrarIntereses :: Float -> Float
cobrarIntereses plata = plata * 1.5

reducirPuestos :: Int -> Estrategia
reducirPuestos cantidadPuestos = (mapActivosPublicos ((-) cantidadPuestos)) . (mapIngresoPerCapita ((*) (reducirIngreso cantidadPuestos)))

reducirIngreso :: Int -> Float
reducirIngreso cantidadPuestos
    | cantidadPuestos > 100 = 0.8      -- se reduce en un 20%
    | otherwise = 0.85               -- se reduce en un 15%

explotar :: String -> Estrategia
explotar recurso pais = pais {deuda = deuda pais - 2, recursosNaturales = quitarRecurso recurso (recursosNaturales pais)}

quitarRecurso :: String -> [String] -> [String]
quitarRecurso recurso recursos = filter (/= recurso) recursos

blindaje :: Estrategia
blindaje pais = (prestarPlata (pbi pais / 2)) . (reducirPuestos 500) $ pais

pbi :: Pais -> Float
pbi pais = (ingresoPerCapita pais) * fromIntegral (poblacionActiva pais)

poblacionActiva :: Pais -> Int 
poblacionActiva pais = activosPrivado pais + activosPublico pais

-- PUNTO 3

recetaEjemplo :: Receta
recetaEjemplo = [prestarPlata 200, explotar "mineria"]

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta receta pais = foldr ($) pais receta
-- en este caso para aplicarle la recetaEjemplo al pais Namibia, en la consola le pasariamos como parametro estos dos, algo asi: aplicarReceta recetaEjemplo namibia.

-- PUNTO 4

puedenZafar :: [Pais] -> [Pais]
puedenZafar = filter $ elem "petroleo" . recursosNaturales

deudaTotal :: [Pais] -> Float
deudaTotal = foldr ((+) . deuda) 0

-- PUNTO 5

estaOrdenado :: Pais -> [Receta] -> Bool
estaOrdenado _ [] = True
estaOrdenado _ [_] = True
estaOrdenado pais (receta1:receta2:recetas) = revisarPBI receta1 pais <= revisarPBI receta2 pais && estaOrdenado pais (receta2:recetas)
    where revisarPBI receta = pbi . aplicarReceta receta

-- PUNTO 6

recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

{-
a) la funcion 4a no funciona ya que la misma busca "petroleo" dentro de los recursosNaturales de los paises, pero si este pais tiene infinitos recursosNaturales nunca va a terminar de tratar de buscar "petroleo"

b) en el caso de la funcion 4b si es posible, ya en ella no nos interesan los recursosNaturales de los paises, solo evalua su deuda, esto es posible gracias a la lazy evaluation.
-}