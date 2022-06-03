import Text.Show.Functions()

data Persona = Persona{
    nombre :: String,
    calorias :: Int,
    hidratacion :: Int,
    tiempoParaEntrenar :: Int,
    equipamiento :: [String]
}deriving(Show, Eq)

type Ejercicio = Persona -> Persona

-- PARTE A

mapNombre :: (String -> String) -> Persona -> Persona
mapNombre unaFuncion unaPersona = unaPersona { nombre = unaFuncion . nombre $ unaPersona }

mapCalorias :: (Int -> Int) -> Persona -> Persona
mapCalorias modificador persona = persona {calorias = modificador.calorias $ persona}

mapHidratacion :: (Int -> Int) -> Persona -> Persona
mapHidratacion modificador persona = persona {hidratacion = modificador.hidratacion $ persona}

menosCaloriasPorRepeticion :: Int -> Int -> Ejercicio
menosCaloriasPorRepeticion valor repeticiones = mapCalorias (flip (-) (valor * repeticiones))

abdominales :: Int -> Ejercicio
abdominales repeticiones persona = menosCaloriasPorRepeticion 8 repeticiones persona

flexiones :: Int -> Ejercicio
flexiones repeticiones = (menosCaloriasPorRepeticion 16 repeticiones) . (mapHidratacion (flip (-) (2 * (repeticiones `div` 10))))

levantarPesas :: Int -> Int -> Ejercicio
levantarPesas peso repeticiones persona
    | elem "pesa" (equipamiento persona) = (menosCaloriasPorRepeticion 32 repeticiones) . (mapHidratacion (flip (-) (peso * (repeticiones `div` 10)))) $ persona
    | otherwise = laGranHomeroSimpson persona

laGranHomeroSimpson :: Ejercicio
laGranHomeroSimpson = id

renovarEquipo :: Persona -> Persona
renovarEquipo persona = persona {equipamiento = map renovar (equipamiento persona)}

renovar :: String -> String
renovar equipo = "Nuevo" ++ equipo

volverseYoguista :: Persona -> Persona
volverseYoguista persona = persona {calorias = calorias persona `div` 2, hidratacion = min 100 (hidratacion persona * 2), equipamiento = ["colchoneta"]}

volverseBodyBuilder :: Persona -> Persona
volverseBodyBuilder persona
    | all (=="pesa") (equipamiento persona) = mapNombre (++ " BB") . mapCalorias (* 3) $ persona
    | otherwise = persona

comerUnSandwich :: Persona -> Persona
comerUnSandwich = (mapCalorias (+500)) . (mapHidratacion (const 100)) 

-- PARTE B

data Rutina = Rutina {
    listaEjercicios :: [Ejercicio],
    duracion :: Int
}deriving(Show)

esPeligrosa :: Persona -> Rutina -> Bool
esPeligrosa persona rutina = estaAgotada.(hacerRutina rutina) $ persona

hacerRutina :: Rutina -> Persona -> Persona
hacerRutina rutina persona
    | duracion rutina < tiempoParaEntrenar persona = foldl (flip ($)) persona (listaEjercicios rutina)
    | otherwise = persona

estaAgotada :: Persona -> Bool
estaAgotada persona = calorias persona < 50 && hidratacion persona < 10

esBalanceada :: Persona -> Rutina -> Bool
esBalanceada persona rutina = estaBalanceado(persona).(hacerRutina rutina) $ persona

estaBalanceado :: Persona -> Persona -> Bool
estaBalanceado personaInicio personaFinal = hidratacion personaFinal > 80 && calorias personaFinal < calorias personaInicio `div` 2

elAbominableAbdominal :: Rutina
elAbominableAbdominal = Rutina {listaEjercicios = (zipWith ($) (repeat abdominales) [1..]), duracion = 60}

-- PARTE C

seleccionarGrupoDeEjercicio :: Persona -> [Persona] -> [Persona]
seleccionarGrupoDeEjercicio persona = filter (tienenElMismoTiempoDisponible persona)

tienenElMismoTiempoDisponible :: Persona -> Persona -> Bool
tienenElMismoTiempoDisponible persona otraPersona = tiempoParaEntrenar persona == tiempoParaEntrenar otraPersona

promedioDeRutina :: Rutina -> [Persona] -> (Int, Int)
promedioDeRutina rutina grupoPersonas = (sacarPromedio.map calorias $ rutinasCompletadas, sacarPromedio.map hidratacion $ grupoPersonas)
    where rutinasCompletadas = map (hacerRutina rutina) grupoPersonas

sacarPromedio :: [Int] -> Int
sacarPromedio lista = div (sum lista) (length lista)