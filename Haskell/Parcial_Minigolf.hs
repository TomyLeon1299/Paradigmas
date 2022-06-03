-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd :: Jugador
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa :: Jugador
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

maximoSegun :: (Foldable t, Ord a1) => (a2 -> a1) -> t a2 -> a2
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord a => (t -> a) -> t -> t -> t
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

type Palo = Habilidad -> Tiro

putter :: Palo
putter unaHabilidad = UnTiro {velocidad = 10, precision = precisionJugador unaHabilidad * 2, altura = 0}

madera :: Palo
madera unaHabilidad = UnTiro {velocidad = 100, precision = precisionJugador unaHabilidad `div` 2, altura = 5}

hierro :: Int -> Palo
hierro n unaHabilidad = UnTiro {velocidad = fuerzaJugador unaHabilidad * n, precision = precisionJugador unaHabilidad `div` n, altura = (n-3) `max` 0}

palos :: [Palo]
palos = [putter,madera] ++ map hierro [1..10]

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

type Obstaculo = Tiro -> Tiro

tiroDetenido :: Tiro
tiroDetenido = UnTiro 0 0 0

obstaculoSuperableSi :: (Tiro -> Bool) -> (Tiro -> Tiro) -> Obstaculo
obstaculoSuperableSi condicion efecto unTiro
    | condicion unTiro = efecto unTiro
    | otherwise = tiroDetenido

tunelConRampita :: Obstaculo
tunelConRampita = obstaculoSuperableSi superaTunelConRampita efectoTunelConRampita

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita unTiro = precision unTiro > 90 && altura unTiro == 0

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita unTiro = unTiro {velocidad = velocidad unTiro * 2, precision = 100, altura = 0}

laguna :: Int -> Obstaculo
laguna largo = obstaculoSuperableSi superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna unTiro = velocidad unTiro > 80 && (between 1 5.altura) unTiro

efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largo unTiro = unTiro {altura = altura unTiro `div` largo}

hoyo :: Obstaculo
hoyo = obstaculoSuperableSi superaHoyo efectoHoyo

superaHoyo :: Tiro -> Bool
superaHoyo unTiro = (between 5 20.velocidad) unTiro && altura unTiro == 0 && precision unTiro > 95

efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = tiroDetenido

-- el punto 4a lo vi en el video y no se entiende nada, hace un data donde adentro hay funciones pero nunca las define