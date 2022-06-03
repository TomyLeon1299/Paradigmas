import Text.Show.Functions()

data Auto = Auto {
    color :: Color,
    velocidad :: Int,
    distancia :: Int
}deriving(Show, Eq)

type Carrera = [Auto]
type Color = String

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

-- PUNTO 1

estaCerca :: Auto -> Auto -> Bool
estaCerca unAuto otroAuto = abs(distancia unAuto - distancia otroAuto) > 10 && unAuto /= otroAuto

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo unAuto unaCarrera = (not . any (estaCerca unAuto)) unaCarrera && lesVaGanandoATodos unAuto unaCarrera

lesVaGanandoATodos :: Auto -> Carrera -> Bool
lesVaGanandoATodos unAuto = all (leVaGanando unAuto).filter (/= unAuto)

leVaGanando :: Auto -> Auto -> Bool
leVaGanando unAuto otroAuto = distancia unAuto > distancia otroAuto

puesto :: Auto -> Carrera -> Int
puesto unAuto = 1 + (length.filter (flip leVaGanando unAuto))

-- PUNTO 2

correr :: Int -> Auto -> Auto
correr tiempo unAuto = unAuto {distancia = distancia unAuto + velocidad unAuto * tiempo}

alterarVelocidad :: (Int -> Int) -> Auto -> Auto
alterarVelocidad modificador unAuto = unAuto {velocidad = modificador.velocidad $ unAuto}

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad cantidad = alterarVelocidad (max 0 . (flip (-) cantidad))

-- PUNTO 4

type PowerUp = Auto -> Carrera -> Carrera

terremoto :: PowerUp
terremoto unAuto unaCarrera = afectarALosQueCumplen (estaCerca unAuto) (bajarVelocidad 50) unaCarrera

miguelitos :: Int -> PowerUp
miguelitos cantidad unAuto unaCarrera = afectarALosQueCumplen (leVaGanando unAuto) (bajarVelocidad cantidad) unaCarrera

jetPack :: Int -> PowerUp
jetPack tiempo unAuto unaCarrera = afectarALosQueCumplen (==unAuto) ((cambiarVelocidad (velocidad unAuto)) . (correr tiempo) . (alterarVelocidad (*2))) unaCarrera

cambiarVelocidad :: Int -> Auto -> Auto
cambiarVelocidad cantidad = alterarVelocidad (\ _ -> cantidad)

-- PUNTO 4

tablaDePosiciones :: Carrera -> [(Int, Color)]
tablaDePosiciones unaCarrera = map (\unAuto -> (puesto unAuto unaCarrera, color unAuto)) unaCarrera

simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)]
simularCarrera unaCarrera eventos = (tablaDePosiciones . foldl (flip ($)) unaCarrera) eventos

correnTodos :: Int -> Carrera -> Carrera
correnTodos tiempo unaCarrera = map (correr tiempo) unaCarrera

usaPowerUp :: Color -> PowerUp -> Carrera -> Carrera
usaPowerUp colorAuto powerUp unaCarrera = (flip powerUp unaCarrera) . (buscarAuto colorAuto) $ unaCarrera

buscarAuto :: Color -> Carrera -> Auto
buscarAuto colorAuto = head . filter ((== colorAuto).color)

carreraDeEjemplo :: Carrera
carreraDeEjemplo = map (\color -> Auto color 120 0) [ "rojo", "blanco", "azul", "negro"]

eventosDeEjemplo :: [Carrera -> Carrera]
eventosDeEjemplo = [correnTodos 30, "azul" `usaPowerUp` (jetPack 3), "blanco" `usaPowerUp` terremoto, correnTodos 40, "blanco" `usaPowerUp` (miguelitos 20), "negro" `usaPowerUp` (jetPack 6), correnTodos 10]

-- PUNTO 5

{-
a) Se puede agregar sin problemas como una función más misilTeledirigido :: Color -> PowerUp, y usarlo como:
usaPowerUp "azul" (misilTeledirigido "rojo") :: Evento

b) - vaTranquilo puede terminar sólo si el auto indicado no va tranquilo
(en este caso por tener a alguien cerca, si las condiciones estuvieran al revés, terminaría si se encuentra alguno al que no le gana).
Esto es gracias a la evaluación perezosa, any es capaz de retornar True si se encuentra alguno que cumpla la condición indicada, y all es capaz de retornar False si alguno no cumple la condición correspondiente. Sin embargo, no podría terminra si se tratara de un auto que va tranquilo.
- puesto no puede terminar nunca porque hace falta saber cuántos le van ganando, entonces por más que se pueda tratar de filtrar el conjunto de autos, nunca se llegaría al final para calcular la longitud.
-}