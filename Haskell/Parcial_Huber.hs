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

data Chofer = Chofer{
    nombre :: String,
    kilometraje :: Int,
    viajes :: [Viaje],
    condicionViaje :: CondicionViaje
}deriving(Show)

data Viaje = Viaje{
    fecha :: (Int, Int, Int),      -- (Dia, Mes, Anio)
    cliente :: Cliente,
    costo :: Int
}deriving(Show, Eq)

data Cliente = Cliente{
    nombreCliente :: String,
    lugar :: String
}deriving(Show, Eq)

type CondicionViaje = Viaje -> Bool

-- PUNTO 2

cualquierViaje :: CondicionViaje
cualquierViaje _ = True

viajesMayorA200 :: CondicionViaje
viajesMayorA200 = (> 200) . costo

clienteNombreLargo :: Int -> CondicionViaje
clienteNombreLargo cantidad = (> cantidad) . length . nombreCliente . cliente

clienteNoViveEn :: String -> CondicionViaje
clienteNoViveEn zona = (/= zona) . lugar . cliente

-- PUNTO 3

lucas :: Cliente
lucas = Cliente {nombreCliente = "Lucas", lugar = "Victoria"}

daniel :: Chofer
daniel = Chofer {nombre = "Daniel", kilometraje = 23500, viajes = [Viaje {fecha = (20, 4, 2017), cliente = lucas, costo = 150}], condicionViaje = clienteNoViveEn "Olivos"}

alejandra :: Chofer
alejandra = Chofer {nombre = "Alejandra", kilometraje = 180000, viajes = [], condicionViaje = cualquierViaje}

-- PUNTO 4

puedeTomarViaje :: Viaje -> Chofer -> Bool
puedeTomarViaje viaje chofer = condicionViaje chofer viaje

-- PUNTO 5

liquidacionChofer :: Chofer -> Int
liquidacionChofer = sum . map costo . viajes

-- PUNTO 6

realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje viaje = hacerViaje viaje . choferConMenosViajes . filter (puedeTomarViaje viaje)

hacerViaje :: Viaje -> Chofer -> Chofer
hacerViaje viaje chofer = chofer {viajes = viaje : viajes chofer}

choferConMenosViajes :: [Chofer] -> Chofer
choferConMenosViajes [] = id
choferConMenosViajes [chofer] = chofer
choferConMenosViajes (chofer1:chofer2:choferes) = choferConMenosViajes (elQueMenosViajesHizo chofer1 chofer2 : choferes)
    

elQueMenosViajesHizo :: Chofer -> Chofer -> Chofer
elQueMenosViajesHizo chofer1 chofer2
    | length (viajes chofer1) > length (viajes chofer2) = chofer2
    | otherwise = chofer1

-- PUNTO 7

nitoInfy :: Chofer
nitoInfy = Chofer {nombre = "Nito Infy", kilometraje = 70000, viajes = viajeInfinitoLucas, condicionViaje = clienteNombreLargo 3}

viajeInfinitoLucas :: [Viaje]
viajeInfinitoLucas = repetirViaje (Viaje {fecha = (11, 3, 2017), cliente = lucas, costo = 50})

repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje

-- b) No se puede calcular la liquidacionChofer de Nito porque tiene infinitos viajes, y nunca terminaria de sumar costos

-- c) Si se puede saber si Nito puede tomar un viaje de Lucas ya que esta accion no toma en cuenta la lista de viajes que Nito ya realizo entonces simplemente se realiza el viaje y listo, esto es posible gracias a la lazy evaluation que usa haskell

-- PUNTO 8

gongNeng :: Ord c => c -> (c -> Bool) -> (a -> c) -> [a] -> c
gongNeng arg1 arg2 arg3 = 
     max arg1 . head . filter arg2 . map arg3