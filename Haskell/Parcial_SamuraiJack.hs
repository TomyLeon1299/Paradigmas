import Text.Show.Functions()

data Elemento = Elemento { 
    tipo :: String,
    ataque :: (Personaje -> Personaje),
    defensa :: (Personaje -> Personaje)
}deriving(Show)

data Personaje = Personaje {
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int
}deriving(Show)

type Transformaciones = Personaje -> Personaje

-- FUNCIONES AUXILIARES

mapAnio :: (Int -> Int) -> Personaje -> Personaje
mapAnio modificador personaje = personaje {anioPresente = modificador.anioPresente $ personaje}

mapSalud :: (Float -> Float) -> Personaje -> Personaje
mapSalud modificador personaje = personaje {salud = modificador.salud $ personaje}

-- PUNTO 1

mandarAlAnio :: Int -> Transformaciones
mandarAlAnio anio = mapAnio (const anio)

meditar :: Transformaciones
meditar = mapSalud (* 1.5)

causarDanio :: Float -> Personaje -> Personaje
causarDanio danio = mapSalud (max 0 . flip (-) danio)

-- PUNTO 2

esMalvado :: Personaje -> Bool
esMalvado personaje = (any (esDeTipo "Maldad") . elementos) personaje

esDeTipo :: String -> Elemento -> Bool
esDeTipo unTipo elemento = tipo elemento == unTipo

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = salud personaje - salud (ataque elemento personaje)

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje enemigos = filter (puedeMatarlo personaje) enemigos

puedeMatarlo :: Personaje -> Personaje -> Bool
puedeMatarlo personaje enemigo = (any (tieneAtaqueMortal personaje) . elementos) enemigo

tieneAtaqueMortal :: Personaje -> Elemento -> Bool
tieneAtaqueMortal personaje elemento = danioQueProduce personaje elemento == salud personaje

-- PUNTO 3

concentracion :: Int -> Elemento
concentracion nivelDeConcentracion = Elemento {tipo = "Magia", ataque = id, defensa = (!! nivelDeConcentracion).(iterate meditar)}

esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados cantidad = replicate cantidad esbirro

esbirro :: Elemento
esbirro = Elemento {tipo = "Maldad", ataque = causarDanio 1, defensa = id}

jack :: Personaje
jack = Personaje {nombre = "Jack", salud = 300, elementos = [concentracion 3, katanaMagica], anioPresente = 200}

katanaMagica :: Elemento
katanaMagica = undefined

aku :: Int -> Float -> Personaje
aku anio saludInicial = Personaje {nombre = "Aku", salud = saludInicial, elementos = (concentracion 4 : esbirrosMalvados (100 * anio) : portalAlFuturo anio), anioPresente = anio}

portalAlFuturo :: Elemento
portalAlFuturo anio = Elemento {tipo = "Magia", ataque = mandarAlAnio anioFuturo, defensa = (aku anioFuturo.salud)}
    where anioFuturo = anio + 2800

-- PUNTO 4

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor
    | salud atacante == 0 = (defensor, atacante)
    | otherwise = luchar proximoAtacante proximoDefensor
    where proximoAtacante = usarElemento ataque defensor (elementos atacante)
          proximoDefensor = usarElemento defensa atacante (elementos atacante)

usarElemento :: (Elemento -> Personaje -> Personaje) -> Personaje -> [Elemento] -> Personaje
usarElemento funcion personaje elementos = foldl (flip ($)) personaje (map funcion elementos)

-- PUNTO 5

f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))

-- ?????