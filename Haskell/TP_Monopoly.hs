import Text.Show.Functions()

data Jugador = UnJugador {
    nombre :: String,
    dinero :: Int,
    tactica :: String,
    propiedadesCompradas :: [Propiedad],
    acciones :: [Accion]
}deriving (Show)

type Accion = Jugador -> Jugador
type Propiedad = (String, Int)    -- (nombre, precio)

carolina :: Jugador
carolina = UnJugador {
    nombre = "Carolina",
    dinero = 500,
    tactica = "Accionista",
    propiedadesCompradas = [],
    acciones = [pasarPorElBanco, pagarAAccionistas]
}

manuel :: Jugador
manuel = UnJugador {
    nombre = "Manuel",
    dinero = 500,
    tactica = "Oferente singular",
    propiedadesCompradas = [],
    acciones = [pasarPorElBanco, enojarse]
}

agregarAccion :: Accion -> Accion
agregarAccion accion unJugador = unJugador {acciones = acciones unJugador ++ [accion]}

pasarPorElBanco :: Accion
pasarPorElBanco unJugador = unJugador {dinero = dinero unJugador + 40, tactica = "Comprador compulsivo"}

enojarse :: Accion
enojarse unJugador = agregarAccion gritar (gritar (unJugador {dinero = dinero unJugador + 50}))

gritar :: Accion
gritar unJugador = unJugador {nombre = "AHHHH" ++ nombre unJugador}

subastar :: Propiedad ->  Accion
subastar (nombrePropiedad, precio) unJugador
    | (elem (tactica unJugador) ["Accionista", "Oferente singular"]) = unJugador {dinero = dinero unJugador - precio, propiedadesCompradas = propiedadesCompradas unJugador ++ [(nombrePropiedad, precio)]}
    | otherwise = unJugador

cobrarAlquiler :: Propiedad -> Int
cobrarAlquiler propiedad | obtenerPrecio propiedad < 150 = 10
                         | otherwise = 20 
 
obtenerPrecio :: Propiedad -> Int
obtenerPrecio propiedad = snd propiedad

cobrarAlquileres :: Accion
cobrarAlquileres unJugador = unJugador {dinero = dinero unJugador + (sum.map cobrarAlquiler $ (propiedadesCompradas unJugador))}

pagarAAccionistas :: Accion
pagarAAccionistas unJugador
    | tactica unJugador == "Accionista" = unJugador {dinero = dinero unJugador + 200}
    | otherwise = unJugador {dinero = dinero unJugador - 100}

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor (nombrePropiedad, precio) unJugador = gritar (unJugador {dinero = dinero unJugador - precio + 10, propiedadesCompradas = propiedadesCompradas unJugador ++ [(nombrePropiedad, precio)]})

ultimaRonda :: Jugador -> Accion
ultimaRonda jugador = foldr1 (.) (reverse.acciones $ jugador)

juegoFinal :: Jugador -> Jugador -> Jugador
juegoFinal jugador1 jugador2 | (dinero.ultimaRonda jugador1 $ jugador1) > (dinero.ultimaRonda jugador2 $ jugador2) = ultimaRonda jugador1 jugador1
                             | otherwise = ultimaRonda jugador2 jugador2

