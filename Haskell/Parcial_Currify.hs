{-
Parcial Funcional PdeP

Fecha:
Nombre: Tomás Guillermo León
Curso: K2001
Legajo: 1713980
DNI: 42101908
-}

import Text.Show.Functions()

-- PUNTO A

data Cancion = Cancion{
    titulo :: String,
    genero :: String,
    duracion :: Int
}deriving(Show, Eq)

data Artista = Artista{
    nombre :: String,
    canciones :: [Cancion],
    efectoPreferido :: Efecto
}deriving(Show)

type Efecto = Cancion -> Cancion

-- Mapeos

mapTitulo :: (String -> String) -> Cancion -> Cancion
mapTitulo unaFuncion cancion = cancion {titulo = unaFuncion . titulo $ cancion}

mapGenero :: (String -> String) -> Cancion -> Cancion
mapGenero unaFuncion cancion = cancion {genero = unaFuncion . genero $ cancion}

mapDuracion :: (Int -> Int) -> Cancion -> Cancion
mapDuracion unaFuncion cancion = cancion {duracion = unaFuncion . duracion $ cancion}

mapNombre :: (String -> String) -> Artista -> Artista
mapNombre unaFuncion artista = artista {nombre = unaFuncion . nombre $ artista}

mapCanciones :: ([Cancion] -> [Cancion]) -> Artista -> Artista
mapCanciones unaFuncion artista  = artista {canciones = unaFuncion . canciones $ artista}

-- Efectos

acortar :: Efecto
acortar = mapDuracion (max 0 . subtract 60)

remixar :: Efecto
remixar = mapTitulo (++ "remix") . mapDuracion (* 2) . mapGenero (const "remixado")

acustizar :: Int -> Efecto
acustizar duracionAcustico cancion
    | genero cancion /= "acustico" = mapGenero (const "acustico") . mapDuracion (const duracionAcustico) $ cancion
    | otherwise = cancion

metaEfecto :: [Efecto] -> Efecto
metaEfecto efectos cancion = foldr ($) cancion efectos

-- Canciones

cafeParaDos :: Cancion
cafeParaDos = Cancion "Café para dos" "rock melancólico" 146

fuiHastaAhi :: Cancion
fuiHastaAhi = Cancion "Fuí hasta ahí" "rock" 279

losEscarabajos :: Artista
losEscarabajos = Artista "Los escarabajos" [rocketRaccoon, mientrasMiBateriaFesteja, tomateDeMadera] acortar

adela :: Artista
adela = Artista "Adela" [teAcordas, unPibeComoVos, daleMechaALaLluvia] remixar

elTigreJoaco :: Artista
elTigreJoaco = Artista "El tigre Joaco" [] (acustizar 360)

rocketRaccoon :: Cancion
rocketRaccoon = undefined

mientrasMiBateriaFesteja :: Cancion
mientrasMiBateriaFesteja = undefined

tomateDeMadera :: Cancion
tomateDeMadera = undefined

teAcordas :: Cancion
teAcordas = undefined

unPibeComoVos :: Cancion
unPibeComoVos = undefined

daleMechaALaLluvia :: Cancion
daleMechaALaLluvia = undefined

-- PARTE B

vistazo :: Artista -> [Cancion]
vistazo artista = take 3 . filter esCorta $ (canciones artista)

esCorta :: Cancion -> Bool
esCorta cancion = duracion cancion < 150

playlist :: String -> [Artista] -> [Cancion]
playlist unGenero artistas = concatMap (cancionesDelGenero unGenero) artistas

cancionesDelGenero :: String -> Artista -> [Cancion]
cancionesDelGenero unGenero artista = filter (esDeGenero unGenero) (canciones artista)

esDeGenero :: String -> Cancion -> Bool
esDeGenero unGenero cancion = genero cancion == unGenero

-- PUNTO C

hacerseDJ' :: Artista -> Artista
hacerseDJ' artista = artista {canciones = map (efectoPreferido artista) (canciones artista)}

hacerseDJ :: Artista -> Artista
hacerseDJ artista = mapCanciones (map (efectoPreferido artista)) artista

tieneGustoHomogeneo' :: Artista -> Bool
tieneGustoHomogeneo' artista = all (esDeGenero.genero.head.canciones $ artista) (canciones artista)

tieneGustoHomogeneo :: Artista -> Bool
tieneGustoHomogeneo artista = sonTodosIguales . map genero . canciones $ artista

sonTodosIguales :: Eq a => [a] -> Bool
sonTodosIguales lista = all (== head lista) lista

formarBanda :: String -> [Artista] -> Artista
formarBanda nombreBanda artistas = Artista {nombre = nombreBanda, canciones = concatMap canciones artistas, efectoPreferido = metaEfecto (map efectoPreferido artistas)}

obraMaestraProgresiva :: Artista -> Cancion
obraMaestraProgresiva artista = Cancion {titulo = concatMap titulo (canciones artista), genero = foldl1 mejorGenero (map genero . canciones $ artista), duracion = sum . map duracion . canciones $ artista}

mejorGenero :: String -> String -> String
mejorGenero "rock" _ = "rock"
mejorGenero _ "rock" = "rock"
mejorGenero "reggaeton" otroGenero = otroGenero
mejorGenero unGenero "reggaeton" = unGenero
mejorGenero unGenero otroGenero
    | length unGenero > length otroGenero = unGenero
    | otherwise = otroGenero

-- PARTE D

{-

1) No, ya que nunca terminaria de aplicar el efecto en la lista de canciones porque la misma es infinita.
2) Si, ya que en lazy evaluation va verificando elemento por elemento de la lista, hasta que se cumpla lo pedido, no es lee la lista entera y despues evalua
3) No, ya que nunca terminaria de evaluar por ejemplo, no puede concatear los titulos ya que son infinitos y nunca terminaria, lo mismo con la duracion de las canciones, a su vez pasa lo mismo con el genero, no puede ver cual es el mejor genero de algo infinito.

-}