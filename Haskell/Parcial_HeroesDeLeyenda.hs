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

type Artefacto = (String, Int)         --(Nombre, Rareza)
type Tarea = Heroe -> Heroe
type Debilidad = Heroe -> Bool
type Labor = [Tarea]

data Heroe = Heroe{
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareas :: [Tarea]
}deriving(Show)

data Bestia = Bestia{
    nombreBestia :: String,
    debilidad :: Debilidad
}deriving(Show)

-- Mapeos

mapEpiteto :: (String -> String) -> Heroe -> Heroe
mapEpiteto unaFuncion heroe = heroe { epiteto = unaFuncion (epiteto heroe) }

mapArtefacto :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe
mapArtefacto unaFuncion heroe = heroe { artefactos = unaFuncion (artefactos heroe) }

mapReconocimiento :: (Int -> Int) -> Heroe -> Heroe
mapReconocimiento unaFuncion heroe = heroe { reconocimiento = unaFuncion (reconocimiento heroe) }

mapRarezaDelArtefacto :: (Int -> Int) -> Artefacto -> Artefacto
mapRarezaDelArtefacto unaFuncion (nombre, rareza) = (nombre, unaFuncion rareza)

mapTarea :: ([Tarea] -> [Tarea]) -> Heroe -> Heroe
mapTarea unaFuncion heroe = heroe { tareas = unaFuncion (tareas heroe) }

-- PUNTO 2

pasaALaHistoria :: Tarea
pasaALaHistoria heroe 
    | reconocimiento heroe > 1000 = cambiarEpitetoPor ( "El mitico") heroe
    | reconocimiento heroe >= 500 = cambiarEpitetoPor ("El magnifico") . aniadirArtefacto ("Lanza del Olimpo", 100) $ heroe
    | reconocimiento heroe < 500 && reconocimiento heroe > 100 = cambiarEpitetoPor ("Hoplita") . aniadirArtefacto ("Xiphos", 50) $ heroe
    | otherwise = heroe

aniadirArtefacto :: Artefacto -> Tarea
aniadirArtefacto artefacto = mapArtefacto (artefacto :)

cambiarEpitetoPor :: String -> Tarea
cambiarEpitetoPor epitetoNuevo = mapEpiteto (const epitetoNuevo)

-- PUNTO 3

encontrarUnArtefacto :: Artefacto -> Tarea
encontrarUnArtefacto artefacto = mapReconocimiento ((+) (snd artefacto)) . aniadirArtefacto artefacto

escalarElOlimpo :: Tarea
escalarElOlimpo = aniadirArtefacto ("El relampago de Zeus", 500) . desecharAlgunosArtefactos . triplicarRarezaDeArtefactos . mapReconocimiento ((+) 500)

triplicarRarezaDeArtefactos :: Tarea
triplicarRarezaDeArtefactos = mapArtefacto (map (mapRarezaDelArtefacto (*3)))

desecharAlgunosArtefactos :: Tarea
desecharAlgunosArtefactos = mapArtefacto (filter ((> 1000) . snd))

ayudarACruzarLaCalle :: Int -> Tarea
ayudarACruzarLaCalle cuadras = cambiarEpitetoPor ("Gros" ++ (take cuadras listaDeOsInfinitas))

listaDeOsInfinitas :: String
listaDeOsInfinitas = 'o' : listaDeOsInfinitas

matarAUnaBestia :: Bestia -> Tarea
matarAUnaBestia bestia heroe
    | (debilidad bestia) heroe = cambiarEpitetoPor ("El asesino de" ++ nombreBestia bestia) heroe
    | otherwise = mapArtefacto (drop 1) . cambiarEpitetoPor ("El cobarde") $ heroe

-- PUNTO 4

heracles :: Heroe
heracles = Heroe {epiteto = "Guardian del Olimpo", reconocimiento = 700, artefactos = [("Pistola", 1000), ("El relampago de Zeus", 500)], tareas = [matarAlLeonDeNemea]}

-- PUNTO 5

leonDeNemea :: Bestia
leonDeNemea = Bestia {nombreBestia = "Leon de Nemea", debilidad = epitetoMayorA20}

epitetoMayorA20 :: Debilidad
epitetoMayorA20 = (> 20) . length . epiteto

matarAlLeonDeNemea :: Tarea
matarAlLeonDeNemea = matarAUnaBestia leonDeNemea

-- PUNTO 6

hacerUnaTarea :: Tarea -> Heroe -> Heroe
hacerUnaTarea tarea heroe = mapTarea (tarea :) . tarea $ heroe

-- PUNTO 7

presumirLogros :: Heroe -> Heroe -> (Heroe, Heroe)
presumirLogros heroe otroHeroe
    | reconocimiento heroe > reconocimiento otroHeroe = (heroe, otroHeroe)
    | reconocimiento heroe < reconocimiento otroHeroe = (otroHeroe, heroe)
    | otherwise = tienenElMismoReconocimiento heroe otroHeroe

tienenElMismoReconocimiento :: Heroe -> Heroe -> (Heroe, Heroe)
tienenElMismoReconocimiento heroe otroHeroe
    | sumatoriaRarezaArtefactos heroe > sumatoriaRarezaArtefactos otroHeroe = (heroe, otroHeroe)
    | sumatoriaRarezaArtefactos heroe < sumatoriaRarezaArtefactos otroHeroe = (otroHeroe, heroe)
    | otherwise = presumirLogros (realizarTareasDelOtro heroe otroHeroe) (realizarTareasDelOtro otroHeroe heroe)

sumatoriaRarezaArtefactos :: Heroe -> Int
sumatoriaRarezaArtefactos = sum . map snd . artefactos

realizarTareasDelOtro :: Heroe -> Heroe -> Heroe
realizarTareasDelOtro heroe otroHeroe = realizarUnaLabor heroe (tareas otroHeroe)

-- PUNTO 8

{-
Esto va a entrar un bucle infinito, ya que en la funcion de presumirLogros va a ir directo al otherwise ya que los dos tienen reconocimiento 100,
dentro de la funcion tienenElMismoReconocimiento tambien entran al otherwise ya que ninguno de los dos tiene artefactos,
entonces va a volver a presumirLogros pero anteriormente se hizo realizarTareasDelOtro pero esto devuelve el mismo heroe ya que ninguno de los dos tiene tereas realizadas.
-}

-- PUNTO 9

realizarUnaLabor :: Heroe -> Labor -> Heroe
realizarUnaLabor heroe labor = foldr (hacerUnaTarea) heroe labor

-- PUNTO 10

listaDeTareasInfinitas :: [Tarea]
listaDeTareasInfinitas = escalarElOlimpo : listaDeTareasInfinitas

{-
realizarUnaLabor heroe listaDeTareasInfinitas

Esto no terminaria nunca ya que como las tareas para hacer son infinitas el heroe nunca terminaria de realizar las mismas.
-}