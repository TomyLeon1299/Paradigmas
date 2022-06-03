import Text.Show.Functions()

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

-- PARTE A

rocketRaccoon :: Cancion
rocketRaccoon = Cancion {titulo = "Rocket Raccoon", genero = "a", duracion = 90}

mientrasMiBateriaFesteja :: Cancion
mientrasMiBateriaFesteja = Cancion {titulo = "Mientras Mi Bateria Festeja", genero = "a", duracion = 90}

tomateDeMadera :: Cancion
tomateDeMadera = Cancion {titulo = "Tomate De Madera", genero = "a", duracion = 90}

teAcordas :: Cancion
teAcordas = Cancion {titulo = "¿Te acordás?", genero = "a", duracion = 90}

unPibeComoVos :: Cancion
unPibeComoVos = Cancion {titulo = "Un pibe como vos", genero = "a", duracion = 90}

daleMechaALaLluvia :: Cancion
daleMechaALaLluvia = Cancion {titulo = "Dale meecha a la lluvia", genero = "a", duracion = 90}

cafeParaDos :: Cancion
cafeParaDos = Cancion {titulo = "Cafe para dos", genero = "rock melancolico", duracion = 146}

fuiHastaAhi :: Cancion
fuiHastaAhi = Cancion {titulo = "Fui hasta ahi", genero = "rock", duracion = 279}

losEscarabajos :: Artista
losEscarabajos = Artista {nombre = "Los escarabajos", canciones = [rocketRaccoon, mientrasMiBateriaFesteja, tomateDeMadera], efectoPreferido = acortar}

adela :: Artista
adela = Artista {nombre = "Adela", canciones = [teAcordas, unPibeComoVos, daleMechaALaLluvia], efectoPreferido = remixar}

elTigreJoaco :: Artista
elTigreJoaco = Artista {nombre = "El tigre Joaco", canciones = [], efectoPreferido = acustizar 360}

-- efectos

acortar :: Efecto
acortar unaCancion = unaCancion { duracion = max 0 (duracion unaCancion - 60)}

remixar :: Efecto
remixar unaCancion = unaCancion { titulo = titulo unaCancion ++ "remix", duracion = duracion unaCancion * 2, genero = "remixado"}

acustizar :: Int -> Efecto
acustizar duracionAcustico unaCancion | genero unaCancion /= "acustico" = unaCancion { genero = "acustico", duracion = duracionAcustico}
                                      | otherwise = unaCancion

metaEfecto :: [Efecto] -> Efecto
metaEfecto listaEfectos = foldr1 (.) listaEfectos

-- PARTE B

vistazo :: Artista -> [Cancion]
vistazo unArtista = take 3 . filter esCorta $ (canciones unArtista)

esCorta :: Cancion -> Bool
esCorta unaCancion = duracion unaCancion < 150

playlist :: String -> [Artista] -> [Cancion]
playlist unGenero artistas = concatMap (cancionesDeGeneroArtista unGenero) artistas

cancionesDeGeneroArtista :: String -> Artista -> [Cancion]
cancionesDeGeneroArtista unGenero unArtista = filter (esDeGenero unGenero) (canciones unArtista)

esDeGenero :: String -> Cancion -> Bool
esDeGenero unGenero unaCancion = genero unaCancion == unGenero

-- PARTE C

hacerseDJ :: Artista -> Artista
hacerseDJ unArtista = unArtista {canciones = map (efectoPreferido unArtista) (canciones unArtista)}

tieneGustoHomogeneo :: Artista -> Bool
tieneGustoHomogeneo unArtista = all (esDeGenero.genero.head.canciones $ unArtista) (canciones unArtista)

formarBanda :: String -> [Artista] -> Artista
formarBanda nombreBanda artistas = Artista {nombre = nombreBanda, canciones = concatMap canciones artistas, efectoPreferido = metaEfecto (map efectoPreferido artistas)}

obraMaestraProgresiva :: Artista -> Cancion
obraMaestraProgresiva unArtista = Cancion {titulo = concatMap titulo (canciones unArtista), genero = (foldl1 mejorGenero (map genero.canciones $ unArtista)) ++ " progresivo", duracion = sum.map duracion.canciones $ unArtista}

mejorGenero :: String -> String -> String
mejorGenero "reggaeton" otroGenero = otroGenero
mejorGenero unGenero "reggaeton" = unGenero
mejorGenero "rock" _ = "rock"
mejorGenero _ "rock" = "rock"
mejorGenero unGenero otroGenero
    | length unGenero > length otroGenero = unGenero
    | otherwise = otroGenero

-- PARTE D

{-

1) No, ya que nunca terminaria de aplicar el efecto en la lista de canciones porque la misma es infinita.
2) Si, ya que en lazy evaluation va verificando elemento por elemento de la lista, hasta que se cumpla lo pedido, no es lee la lista entera y despues evalua
3) No, ya que nunca terminaria de evaluar por ejemplo, no puede concatear los titulos ya que son infinitos y nunca terminaria, lo mismo con la duracion de las canciones, a su vez pasa lo mismo con el genero, no puede ver cual es el mejor genero de algo infinito.

-}