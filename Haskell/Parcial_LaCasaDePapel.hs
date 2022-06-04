{-
Parcial Funcional PdeP

Fecha:
Nombre: Tomás Guillermo León
Curso: K2001
Legajo: 1713980
DNI: 42101908
-}

import Text.Show.Functions()

-- Modelado

data Ladron = Ladron{
  nombreLadron :: String,
  habilidades :: [String],
  armas :: [Arma]
}deriving(Show)

data Rehen = Rehen{
  nombreRehen :: String,
  complot :: Int,
  miedo :: Int,
  plan :: Plan
}deriving(Show)

type Rebelar = Ladron -> Ladron
type Plan = [Rebelar]
type Arma = Rehen -> Rehen

-- Mapeo

mapNombreLadron :: (String -> String) -> Ladron -> Ladron
mapNombreLadron unaFuncion ladron = ladron { nombreLadron = unaFuncion (nombreLadron ladron) }

mapArmas :: ([Arma] -> [Arma]) -> Ladron -> Ladron
mapArmas unaFuncion ladron = ladron { armas = unaFuncion (armas ladron) }

mapNombreRehen :: (String -> String) -> Rehen -> Rehen
mapNombreRehen unaFuncion rehen = rehen { nombreRehen = unaFuncion (nombreRehen rehen) }

mapComplot :: (Int -> Int) -> Rehen -> Rehen
mapComplot unaFuncion rehen = rehen { complot = unaFuncion (complot rehen) }

mapMiedo :: (Int -> Int) -> Rehen -> Rehen
mapMiedo unaFuncion rehen = rehen { miedo = unaFuncion (miedo rehen) }

-- Armas

pistola :: Int -> Arma
pistola calibre rehen = mapComplot ((-) (5*calibre)) . mapMiedo ((+) ((*3) . length . nombreRehen $ rehen)) $ rehen

ametralladora :: Int -> Arma
ametralladora balas = mapComplot (`div` 2) . mapMiedo (* balas)

-- Intimidar

disparos :: Ladron -> Arma
disparos ladron rehen = (armaMasIntimidante (armas ladron) rehen) rehen

armaMasIntimidante :: [Arma] -> Rehen -> Arma
armaMasIntimidante [] _ = undefined
armaMasIntimidante (x:[]) _ = x
armaMasIntimidante (x:xs) rehen
  | miedo (x rehen) > (miedo . (head xs) $ rehen) = armaMasIntimidante (x:(drop 1 xs)) rehen
  | otherwise = armaMasIntimidante xs rehen

hacerseElMalo :: Ladron -> Rehen -> Rehen
hacerseElMalo ladron rehen
  | nombreLadron ladron == "Berlin" = mapMiedo ((+) (length $ foldl1 (++) (habilidades ladron))) rehen
  | nombreLadron ladron == "Rio" = mapComplot ((+) 20) rehen
  | otherwise = mapMiedo ((+) 10) rehen

-- Rehenes intentan rebelarse contra los ladrones

atacarAlLadron :: Rehen -> Rebelar
atacarAlLadron companiero = mapArmas (drop ((length . nombreRehen $ companiero) `div` 10))

esconderse :: Rebelar
esconderse ladron = mapArmas (drop ((length . habilidades $ ladron) `div` 3)) ladron

-- SEGUNDA PARTE

-- 1.

tokio :: Ladron
tokio = Ladron "tokio" ["trabajo psicologico", "entrar en moto"] [pistola 9, ametralladora 30]

profesor :: Ladron
profesor = Ladron "profesor" ["disfrazarse de linyera", "disfrazarse de payaso", "estar siempre un paso adelante"] []

pablo :: Rehen
pablo = Rehen "pablo" 40 30 [esconderse]

arturito :: Rehen
arturito = Rehen "arturito" 70 50 [esconderse, atacarAlLadron pablo]

-- 2.

esInteligente :: Ladron -> Bool
esInteligente ladron = (nombreLadron ladron == "profesor") || length (habilidades ladron) > 2

-- 3.

consigueArmaNueva :: Arma -> Ladron -> Ladron
consigueArmaNueva armaNueva = mapArmas (++ [armaNueva])

-- 4.

imitarA :: Rehen -> Ladron -> Ladron
imitarA rehen ladron = undefined

-- 5.

calmarLasAguas :: Ladron -> [Rehen] -> [Rehen]
calmarLasAguas ladron rehenes = undefined

-- 6.

puedeEscaparse :: Ladron -> Bool
puedeEscaparse ladron = elem "disfrazarse de" (map (take 14) (habilidades ladron))

-- 7.

laCosaPintaMal :: [Ladron] -> [Rehen] -> Bool
laCosaPintaMal ladrones rehenes = (complotPromedio rehenes) > ((miedoPromedio rehenes) * (length . map armas $ ladrones))

complotPromedio :: [Rehen] -> Int
complotPromedio rehenes = (sum . map complot $ rehenes) `div` (length rehenes)

miedoPromedio :: [Rehen] -> Int
miedoPromedio rehenes = (sum . map miedo $ rehenes) `div` (length rehenes)

-- 8.

--seRebelen :: [Rehen] -> Ladron -> Plan -> 

-- 9.

planValencia :: Plan
planValencia = undefined

-- 10.
-- No, ya que cuando se quiera ver la cantidad de armas que tienen los ladrones no va a terminar nunca ya que uno tiene infinitas armas, entonces nunca va a terminar de contar dichas armas

-- 11.
-- Si, ya que el planValencia no usa las habilidades de los ladrones, solo usa las armas, entonces que un ladron tenga infinitas habilidades no importa, ya que gracias a la lazy evaluation la funcion solo va a buscar las armas de los ladrones y no le interesa sus habilidades

-- 12.

funcion :: Foldable t1 => t2 -> (a1 -> t1 a2) -> (t2 -> a1 -> Bool) -> Int -> [a1] -> Bool
funcion cond num lista str = (> str) . sum . map (length . num) . filter (lista cond)
