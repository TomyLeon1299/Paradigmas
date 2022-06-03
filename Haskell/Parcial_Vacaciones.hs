import Text.Show.Functions()

data Turista = Turista {
    cansancio :: Int,
    stress :: Int,
    viajaSolo :: Bool,
    idiomas :: [Idioma]
}deriving(Show, Eq)

type Idioma = String

-- PUNTO 1

ana :: Turista
ana = Turista {cansancio = 0, stress = 21, viajaSolo = False, idiomas = ["espaniol"]}

beto :: Turista
beto = Turista {cansancio = 15, stress = 15, viajaSolo = True, idiomas = ["aleman", "catalan"]}

cathi :: Turista
cathi = Turista {cansancio = 15, stress = 15, viajaSolo = True, idiomas = ["aleman"]}

cambiarStress :: Int -> Excursion
cambiarStress numero turista = turista {stress = stress turista + numero}

cambiarCansancio :: Int -> Excursion
cambiarCansancio numero turista = turista {cansancio = cansancio turista + numero}

aprenderIdioma :: [Idioma] -> Excursion
aprenderIdioma idiomaNuevo turista = turista {idiomas = idiomas turista ++ idiomaNuevo}

acompaniado :: Excursion
acompaniado turista = turista {viajaSolo = False}

cambiarStressPorcentual :: Int -> Excursion
cambiarStressPorcentual porciento turista = cambiarStress (div (porciento * stress turista) 100) turista

-- PUNTO 2

type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya turista | viajaSolo turista == True = cambiarCansancio (-5) turista
                   | otherwise = cambiarStress (-1) turista

apreciarPaisaje :: String -> Excursion
apreciarPaisaje elemento turista = cambiarStress (- length elemento) turista

salirAHablarUnIdioma :: Idioma -> Excursion
salirAHablarUnIdioma idioma turista = acompaniado . (aprenderIdioma [idioma]) $ turista

caminar :: Int -> Excursion
caminar minutos = (cambiarStress (- intensidad minutos)) . (cambiarCansancio (- intensidad minutos))

intensidad :: Int -> Int
intensidad minutos = div minutos 4

paseoEnBarco :: String -> Excursion
paseoEnBarco marea turista | marea == "fuerte" = (cambiarStress 6) . (cambiarCansancio 10) $ turista
                           | marea == "tranquila" = (caminar 10).(apreciarPaisaje "mar").(aprenderIdioma ["aleman"]) $ turista
                           | otherwise = turista        -- si la marea es moderada o el usuario pone otra cosa

-- a)
hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion turista = cambiarStressPorcentual (-10) . excursion $ turista

-- b)
deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun funcion turista excursion = deltaSegun funcion (hacerExcursion excursion turista) turista

-- c)
esEducativa :: Turista -> Excursion -> Bool
esEducativa turista excursion = length (idiomas turista) < length (idiomas (hacerExcursion excursion turista))

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista = filter (esDesestresante turista)

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista = (<= -3) . (deltaExcursionSegun stress turista)

-- PUNTO 3

type Tour = [Excursion]

completo :: Tour
completo = [caminar 20, apreciarPaisaje "cascada", caminar 40, irALaPlaya, salirAHablarUnIdioma "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB excursion = [paseoEnBarco "tranquila", excursion, caminar 120]

islaVecina :: String -> Tour
islaVecina mareaVecina = [paseoEnBarco mareaVecina, excursionVecina mareaVecina, paseoEnBarco mareaVecina]

excursionVecina :: String -> Excursion
excursionVecina "fuerte" = apreciarPaisaje "lago"
excursionVecina _ = irALaPlaya

-- a)
hacerTour :: Turista -> Tour -> Turista
hacerTour turista tour = foldl (flip hacerExcursion) (cambiarStress (length tour) turista) tour

-- b)
propuestaConvincente :: Turista -> [Tour] -> Bool
propuestaConvincente turista = any (esConvincente turista)

esConvincente :: Turista -> Tour -> Bool
esConvincente turista = any (dejaAcompaniado turista) . excursionesDesestresantes turista

dejaAcompaniado :: Turista -> Excursion -> Bool
dejaAcompaniado turista = not . viajaSolo . flip hacerExcursion turista

-- c)
efectividad :: Tour -> [Turista] -> Int
efectividad tour = sum . map (espiritualidadRecibida tour) . filter (flip esConvincente tour)

espiritualidadRecibida :: Tour -> Turista -> Int
espiritualidadRecibida tour = negate . deltaRutina tour

deltaRutina :: Tour -> Turista -> Int
deltaRutina tour turista = deltaSegun nivelDeRutina (hacerTour turista tour) turista

nivelDeRutina :: Turista -> Int
nivelDeRutina turista = cansancio turista + stress turista

-- PUNTO 4

-- a)
playasInfinitas :: Tour
playasInfinitas = irALaPlaya : repeat irALaPlaya

-- b)
{-
Ana no viaja sola, ya se cumple una condicion, y ademas la primera excursion de playasInfinitas ya es desestresante, entonces se cumplen las dos condiciones, y por ende el tour es convincente.

Beto viaja solo y ninguna de las excursiones de playasInfinitas hace que viaje acompañado, entonces el algoritmo diverge.
-}

-- c) La unica forma de saber si ese tour es efectivo es que la lista de turistas este vacia, si no es asi es imposible, ya que nunca termina de verificar la efectividad del tour ya que es infinito.